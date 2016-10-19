module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (checked, class, href, id, name, type')
import Html.Events exposing (onClick)
import Json.Decode exposing ((:=), int, list, string, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Ordering exposing (Ordering, breakTiesWith)
import Regex
import String


{-| The elm architecture revolves around a value of a user-defined type `Model` that starts with
an initial value `init` and is rendered to HTML with a function called `view` of type
`Model -> Html Msg`. Note that `Html` is a parameterized type: that means the HTML fragment can
produce "messages" of type `Msg`, which is also a user-defined types. Messages are then processed
by a function `update` of type `Msg -> Model -> Model` which takes the previous model plus a
message and produces a new world.

`main` is responsible for tying up all those pieces and acting as an entry point for the program.
The three extra things going on here are:

* This program actually takes "flags", which really means that on initialization it takes a value
  provided by Javascript that it can use to build its initial `Model` value. Here we use that to
  take the course list, which we receive as a JSON value that we parse. The `init` argument
  is where that happens.

* There's this funny thing where I say "! []" a couple of times. This is saying that my program
  never has any commands that it wants to ask the elm runtime to perform. Effects are things like
  fetching a resource over HTTP. This program doesn't need any of that.

* There's an input called "subscriptions". This is how an elm program can get notified of external
  inputs like mouse movements and such that aren't tied to direct interactions with our rendered
  HTML. Again, our program doesn't need any of that so I've provided a stub.
-}
main : Program Json.Decode.Value
main =
    App.programWithFlags
        { init = \v -> init v ! []
        , update = \msg model -> update msg model ! []
        , subscriptions = \_ -> Sub.none
        , view = view
        }


{-| Our model consists of:

* All the courses
* The sorted, filtered list of courses that are displayed. (We don't need to do
  this -- we could sort and filter in the view function -- but I didn't want to
  have to do a potentially heavy-weight computation every render.)
* The settings of the form (what buttons are checked and so on)
* If there is an error message to display, what it is
-}
type alias Model =
    { allCourses : List Course
    , displayedCourses : List CourseView
    , settings : Settings
    , error : Maybe String
    }


type alias Settings =
    { filter : Filter
    , sort : SortOrder
    , showLabInfo : Bool
    }


type alias Course =
    { classUrl : Maybe String
    , department : String
    , title : String
    , description : String
    , instructor : String
    , instructorUrl : Maybe String
    , isCore : Bool
    , isElective : Bool
    , isGrad : Bool
    , isSeminar : Bool
    , isTheory : Bool
    , labs : Maybe String
    , limit : Maybe Int
    , location : String
    , meetingTime : MeetingTime
    , offeringDescription : Maybe String
    , year : Year
    , quarter : Quarter
    , number : Int
    , section : Int
    , level : String
    , prereqs : Maybe String
    , registrar : Maybe String
    }


type alias CourseView =
    { course : Course
    , descriptionVisible : Bool
    }


type Quarter
    = Summer
    | Autumn
    | Winter
    | Spring


type alias Year =
    Int


type alias MeetingTime =
    { days : List Day
    , text : String
    }


type Day
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


type alias Filter =
    { selectedQuarters : List Quarter
    , selectedLevels : List String
    , selectedDays : List Day
    , departmentFilter :
        Maybe String
        -- "Nothing" means all depts
    }


type SortOrder
    = QuarterAndNumber
    | Department
    | Instructor
    | Title
    | Location


initFilter : Filter
initFilter =
    { selectedQuarters = [ Summer, Autumn, Winter, Spring ]
    , selectedLevels = [ "100", "200", "300", "400", "500" ]
    , selectedDays = [ Monday, Tuesday, Wednesday, Thursday, Friday ]
    , departmentFilter = Nothing
    }


initSort : SortOrder
initSort =
    QuarterAndNumber


{-| Converts filter settings into a predicate on courses.

This is a fairly direct port of the logic in the original program.
-}
courseFilter : Filter -> Course -> Bool
courseFilter { selectedQuarters, selectedLevels, selectedDays, departmentFilter } =
    oConj
        [ List.map toQuarterPred selectedQuarters |> oDisj
        , List.map toLevelPred selectedLevels |> oDisj
        , List.map toDayPred selectedDays |> oDisj
        , toDeptPred departmentFilter
        ]


matchField : (b -> a) -> a -> b -> Bool
matchField selector target overallValue =
    (selector overallValue) == target


toQuarterPred : Quarter -> Course -> Bool
toQuarterPred =
    matchField .quarter


toLevelPred : String -> Course -> Bool
toLevelPred =
    matchField .level


toDayPred : Day -> Course -> Bool
toDayPred day course =
    List.member day course.meetingTime.days


toDeptPred : Maybe String -> Course -> Bool
toDeptPred deptCriterion course =
    case deptCriterion of
        Nothing ->
            True

        Just dept ->
            course.department == dept


oConj : List (a -> Bool) -> a -> Bool
oConj preds item =
    List.all (\pred -> pred item) preds


oDisj : List (a -> Bool) -> a -> Bool
oDisj preds item =
    List.any (\pred -> pred item) preds


compareQuarters : Ordering Quarter
compareQuarters =
    Ordering.explicit [ Summer, Autumn, Winter, Spring ]


allEqualComparator : Ordering a
allEqualComparator x y =
    EQ


standardCourseSort : Ordering Course
standardCourseSort =
    Ordering.byField .year
        |> breakTiesWith (Ordering.byFieldWith compareQuarters .quarter)
        |> breakTiesWith (Ordering.byField .department)
        |> breakTiesWith (Ordering.byField .number)
        |> breakTiesWith (Ordering.byField .section)


courseSort : SortOrder -> Ordering Course
courseSort sortOrder =
    let
        overridingOrder =
            case sortOrder of
                QuarterAndNumber ->
                    allEqualComparator

                Department ->
                    Ordering.byField .department

                Instructor ->
                    Ordering.byField .instructor

                Title ->
                    Ordering.byField .title

                Location ->
                    Ordering.byField .location
    in
        overridingOrder |> breakTiesWith standardCourseSort


{-| The actions that a user might perform by interacting with the page. At the top
level it's just updating one of the settings or toggling the description on one
of the course listings.
-}
type Msg
    = UpdateSettings SettingsMsg
    | ToggleDescriptionVisibility Course


type SettingsMsg
    = UpdateFilter FilterMsg
    | SelectOrdering SortOrder
    | ToggleShowLabInfo
    | ResetFilters


type FilterMsg
    = ToggleQuarterSelected Quarter
    | ToggleLevelSelected String
    | ToggleDaySelected Day
    | SelectDepartment (Maybe String)


type SelectedState
    = Selected
    | NotSelected


{-| Takes a message and the current state of the world and produces an updated state
of the world. Since we bake the visible courses into the model, we need to make sure
we do that here any time we update our settings.
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSettings settingsMsg ->
            let
                settings =
                    updateSettings settingsMsg model.settings

                filter =
                    courseFilter settings.filter

                sort =
                    courseSort settings.sort

                displayedCourses =
                    model.allCourses
                        |> List.filter filter
                        |> List.sortWith sort
                        |> List.map (\c -> CourseView c False)
            in
                { model
                    | settings = settings
                    , displayedCourses = displayedCourses
                }

        ToggleDescriptionVisibility course ->
            let
                newCourses =
                    updateFirstMatch
                        (\courseView -> courseView.course == course)
                        (\courseView -> { courseView | descriptionVisible = not courseView.descriptionVisible })
                        model.displayedCourses
            in
                { model | displayedCourses = newCourses }


{-| Utility function: given a predicate and an "update" function, finds the first
element in the given list that matches the predicate and replaces it with the
result of the update function within the list.

We use this in `ToggleDescriptionVisibility` above to find a course to toggle
visibility for, and then update it with the toggled visibility.
-}
updateFirstMatch : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirstMatch pred updater items =
    let
        updateInternal items =
            case items of
                x :: xs ->
                    if pred x then
                        (updater x) :: xs
                    else
                        x :: (updateInternal xs)

                [] ->
                    []
    in
        updateInternal items


updateSettings : SettingsMsg -> Settings -> Settings
updateSettings settingsMsg settings =
    case settingsMsg of
        UpdateFilter filterMsg ->
            { settings | filter = updateFilter filterMsg settings.filter }

        SelectOrdering sortOrder ->
            { settings | sort = sortOrder }

        ToggleShowLabInfo ->
            { settings | showLabInfo = not settings.showLabInfo }

        ResetFilters ->
            { settings | filter = initFilter }


updateFilter : FilterMsg -> Filter -> Filter
updateFilter filterMsg filter =
    case filterMsg of
        ToggleQuarterSelected quarter ->
            { filter | selectedQuarters = toggleFilterList quarter filter.selectedQuarters }

        ToggleLevelSelected level ->
            { filter | selectedLevels = toggleFilterList level filter.selectedLevels }

        ToggleDaySelected day ->
            { filter | selectedDays = toggleFilterList day filter.selectedDays }

        SelectDepartment newDepartmentFilter ->
            { filter | departmentFilter = newDepartmentFilter }


toggleFilterList : a -> List a -> List a
toggleFilterList item items =
    if List.member item items then
        List.filter (\x -> x /= item) items
    else
        item :: items


{-| Renders the model into HTML.

Elm provides functions that directly emit messages from the HTML result value
in response to clicks or other events, but otherwise is basically a direct port
of HTML elements into Elm. It is easy to write "UI components" as simple functions,
though, and you see examples of that in things like `labeledGroup`.
-}
view : Model -> Html Msg
view model =
    let
        numDisplayedCourses =
            List.length model.displayedCourses

        coursePhrase =
            if numDisplayedCourses == 1 then
                "1 course meets"
            else
                (toString numDisplayedCourses) ++ " courses meet"

        courseText =
            coursePhrase ++ " these criteria"
    in
        div [ id "headermatter" ]
            [ h1 [] [ text "Computer Science Course Offerings 2016-2017" ]
            , h2 [] [ text "The University of Chicago" ]
            , p []
                (viewSettings model.settings
                    :: (case model.error of
                            Nothing ->
                                []

                            Just errorMessage ->
                                []
                       )
                )
            , p []
                [ a
                    [ class "sitenav"
                    , href "http://course-info.cs.uchicago.edu"
                    ]
                    [ text "course info home" ]
                , text " | "
                , a
                    [ class "sitenav"
                    , href "./instructors-quarters-1617.html"
                    ]
                    [ text "instructors by quarter" ]
                , text " | "
                , a
                    [ class "sitenav"
                    , href "./compact.html"
                    ]
                    [ text "compact view" ]
                ]
            , p []
                [ p []
                    [ i []
                        [ small []
                            [ text courseText
                            ]
                        ]
                    ]
                , viewTable model.settings.showLabInfo model.displayedCourses
                ]
            ]


{-| Renders a checkbox with a label.
-}
checkbox : String -> Msg -> Bool -> Html Msg
checkbox label msg isChecked =
    span []
        [ input
            [ type' "checkbox"
            , onClick msg
            , checked isChecked
            ]
            []
        , text label
        ]


{-| Renders a box with a label and the given contents.
-}
labeledGroup : String -> List (Html msg) -> Html msg
labeledGroup label body =
    fieldset []
        ((legend [] [ text label ])
            :: body
        )


{-| Renders a labeled checkbox group with the given labels.

You additionally provide a list of values (`options`) that conceptually be checked
along with display names for each. You also provide a list of the ones that are currently
checked, and a function for deciding what message to emit to report the checking or
unchecking of a given option.
-}
checkboxGroup : String -> List a -> (a -> FilterMsg) -> List ( String, a ) -> Html Msg
checkboxGroup label selectedOptions toMsg options =
    labeledGroup label
        (List.map
            (uncurry
                (\label option ->
                    checkbox
                        label
                        (UpdateSettings <| UpdateFilter <| toMsg option)
                        (List.member option selectedOptions)
                )
            )
            options
        )


{-| Renders a radio button.
 -}
radioBox : String -> String -> Msg -> Bool -> Html Msg
radioBox groupName label msg isSelected =
    span []
        [ input
            [ type' "radio"
            , checked isSelected
            , onClick msg
            ]
            []
        , text label
        ]


{-| Renders a box with radio buttons. Arguments are similar to `checkboxGroup` above.
 -}
radioGroup : String -> String -> a -> (a -> Msg) -> List ( String, a ) -> Html Msg
radioGroup label groupName selection toMsg options =
    labeledGroup label
        (List.map
            (uncurry
                (\label option ->
                    radioBox
                        groupName
                        label
                        (toMsg option)
                        (option == selection)
                )
            )
            options
        )


quarterText : Quarter -> String
quarterText quarter =
    case quarter of
        Summer ->
            "Summer"

        Autumn ->
            "Autumn"

        Winter ->
            "Winter"

        Spring ->
            "Spring"


viewCourseRows : Bool -> CourseView -> List (Html Msg)
viewCourseRows showLabs courseView =
    let
        course =
            courseView.course
    in
        (tr []
            [ td [] [ text (quarterText course.quarter ++ " " ++ (toString course.year)) ]
            , td []
                [ viewCourse showLabs courseView
                ]
            , td []
                [ viewInstructor course ]
            , td []
                [ viewMeetingTime course.meetingTime ]
            ]
        )
            :: (case ( showLabs, course.labs ) of
                    ( True, Just labsText ) ->
                        [ tr [ class "labs" ]
                            [ td [] []
                            , td [ class "labs" ]
                                [ text (toString course.number ++ " labs: " ++ labsText) ]
                            ]
                        ]

                    _ ->
                        []
               )


viewCourse : Bool -> CourseView -> Html Msg
viewCourse showLabs courseView =
    let
        course =
            courseView.course
    in
        span []
            ([ courseTitle course
             , a [ onClick (ToggleDescriptionVisibility course) ] [ text " ~ " ]
             ]
                ++ (if courseView.descriptionVisible then
                        [ blockquote []
                            (text course.description
                                :: (Maybe.map
                                        (\offeringDescription ->
                                            [ blockquote [] [ text offeringDescription ] ]
                                        )
                                        course.offeringDescription
                                        |> Maybe.withDefault []
                                   )
                            )
                        ]
                    else
                        []
                   )
                ++ (if showLabs then
                        []
                    else
                        []
                   )
                ++ [ blockquote [ class "prereqs" ]
                        [ text "Prereqs: "
                        , text (Maybe.withDefault "none" course.prereqs)
                        ]
                   ]
            )


courseTitle : Course -> Html msg
courseTitle course =
    let
        titleText =
            course.department
                ++ " "
                ++ (toString course.number)
                ++ "-"
                ++ (toString course.section)
                ++ ": "
                ++ course.title
    in
        case course.classUrl of
            Nothing ->
                text titleText

            Just url ->
                a [ href url ] [ text titleText ]


viewInstructor : Course -> Html msg
viewInstructor course =
    case course.instructorUrl of
        Nothing ->
            text course.instructor

        Just url ->
            a [ href url ] [ text course.instructor ]


viewMeetingTime : MeetingTime -> Html msg
viewMeetingTime meetingTime =
    text meetingTime.text


viewTable : Bool -> List CourseView -> Html Msg
viewTable showLabs courses =
    div [ class "scroll" ]
        [ p []
            [ table []
                ((tr []
                    (List.map
                        (\header -> th [] [ text header ])
                        [ "quarter"
                        , "title/prereqs (click ~ for description)"
                        , "instructor"
                        , "meets"
                        ]
                    )
                 )
                    :: (List.concatMap (viewCourseRows showLabs) courses)
                )
            ]
        ]


viewSettings : Settings -> Html Msg
viewSettings settings =
    form [ class "form" ]
        [ checkboxGroup
            "Quarter(s)"
            settings.filter.selectedQuarters
            ToggleQuarterSelected
            [ ( "Summer", Summer )
            , ( "Autumn", Autumn )
            , ( "Winter", Winter )
            , ( "Spring", Spring )
            ]
        , checkboxGroup
            "Level(s)"
            settings.filter.selectedLevels
            ToggleLevelSelected
            [ ( "100", "100" )
            , ( "200", "200" )
            , ( "300", "300" )
            , ( "400", "400" )
            , ( "500", "500" )
            ]
        , checkboxGroup
            "Days (of lectures)"
            settings.filter.selectedDays
            ToggleDaySelected
            [ ( "M", Monday )
            , ( "T", Tuesday )
            , ( "W", Wednesday )
            , ( "R", Thursday )
            , ( "F", Friday )
            ]
        , radioGroup
            "Department (cross-listings not guaranteed)"
            "radioDept"
            settings.filter.departmentFilter
            (UpdateSettings << UpdateFilter << SelectDepartment)
            [ ( "All", Nothing )
            , ( "CAPP", Just "CAPP" )
            , ( "CMSC", Just "CMSC" )
            , ( "MPCS", Just "MPCS" )
            , ( "TTIC", Just "TTIC" )
            ]
        , br [] []
        , radioGroup "Order By"
            "radioOrder"
            settings.sort
            (UpdateSettings << SelectOrdering)
            [ ( "Quarter, Number", QuarterAndNumber )
            , ( "Dept", Department )
            , ( "Instructor", Instructor )
            , ( "Title", Title )
            , ( "Location", Location )
            ]
        , labeledGroup "Show/Hide"
            [ checkbox "labs" (UpdateSettings ToggleShowLabInfo) (settings.showLabInfo)
            ]
        , button
            [ onClick (UpdateSettings ResetFilters)
            , type' "button"
            ]
            [ text "Display All Courses" ]
        ]


{-| The last piece: A function that takes a raw JSON value (the contents of
`course-data.js`) and turns it into a model. The only interesting thing here is
how we parse the JSON value using JSON parser combinators for the data format.
-}
init : Json.Decode.Value -> Model
init jsonValue =
    case Json.Decode.decodeValue ("courses" := list parseCourse) jsonValue of
        Ok allCourses ->
            { allCourses = allCourses
            , displayedCourses =
                allCourses
                    |> List.sortWith standardCourseSort
                    |> List.map (\c -> CourseView c False)
            , settings =
                { filter = initFilter
                , sort = initSort
                , showLabInfo = False
                }
            , error = Nothing
            }

        Err errorMessage ->
            { allCourses = []
            , displayedCourses = []
            , settings =
                { filter = initFilter
                , sort = initSort
                , showLabInfo = False
                }
            , error = Just errorMessage
            }

{-| Decoder for courses.

In Elm, the `decodeValue` function (used above) is responsible for parsing
JSON values, using a `Decoder` to guide it. This decoder decodes courses.
As you can see, it's mostly just compositionally reading off fields --
each entry specifies a field we expect to find in the JSON record for a
course and, recursively, a decoder to read that field. Most are just "string"
but a few are more interesting.
 -}
parseCourse : Decoder Course
parseCourse =
    decode Course
        |> required "classURL" (noneOr string)
        |> required "dept" string
        |> required "title" string
        |> required "descrip" string
        |> required "inst" string
        |> required "instructorURL" (noneOr string)
        |> required "isCore" booleanish
        |> required "isElective" booleanish
        |> required "isGrad" booleanish
        |> required "isSeminar" booleanish
        |> required "is_theory" booleanish
        |> required "labs" (noneOr string)
        |> required "limit" (noneOr int)
        |> required "loc" string
        |> required "meets" parseMeetingTime
        |> required "offeringDescrip" (noneOr string)
        |> required "quarter" parseYearFromYearQuarter
        |> required "quarter" parseQuarterFromYearQuarter
        |> required "num" int
        |> required "sec" int
        |> required "num" parseLevelFromNum
        |> required "prerequisites" (noneOr string)
        |> required "registrar" (noneOr string)


{-| A parser that succeeds only if the JSON value provided decodes
with the given decoder to exactly the given value. If so, this decoder
produces that value; otherwise it fails.

We use this to look for specific values like the string "None".
 -}
parseConstant : Decoder a -> a -> Decoder a
parseConstant decoder constant =
    decoder
        `Json.Decode.andThen`
            (\s ->
                if s == constant then
                    Json.Decode.succeed constant
                else
                    Json.Decode.fail ("Not " ++ (toString constant))
            )

{-| Parses year number from a string like "Autumn 2016"
-}
parseYearFromYearQuarter : Decoder Int
parseYearFromYearQuarter =
    let
        yearRegex =
            Regex.regex "[0-9]+"
    in
        Json.Decode.customDecoder
            string
            (\yearQuarter ->
                Regex.find (Regex.AtMost 1) yearRegex yearQuarter
                    |> List.map .match
                    |> listToResultWithFailureMessage "No number"
                    |> \r -> r `Result.andThen` String.toInt
            )

{-| Utility function that converts a list to a single-value success
if the list contains exactly one item, or an error otherwise.
 -}
listToResultWithFailureMessage : err -> List a -> Result err a
listToResultWithFailureMessage error items =
    case items of
        [ item ] ->
            Ok item

        _ ->
            Err error

{-| Parses quarter from a string like "Autumn 2016"

 -}
parseQuarterFromYearQuarter : Decoder Quarter
parseQuarterFromYearQuarter =
    let
        yearQuarterToQuarter yearQuarter =
            case String.split " " yearQuarter of
                "Summer" :: _ ->
                    Just Summer

                "Autumn" :: _ ->
                    Just Autumn

                "Winter" :: _ ->
                    Just Winter

                "Spring" :: _ ->
                    Just Spring

                _ ->
                    Nothing
    in
        string
            `Json.Decode.andThen`
                (\yearQuarter ->
                    yearQuarterToQuarter yearQuarter
                        |> Maybe.map Json.Decode.succeed
                        |> Maybe.withDefault (Json.Decode.fail "Not a quarter")
                )


{-| Parses the level ("200", "300", etc) from a course number (e.g. 23000, 31400).
 -}
parseLevelFromNum : Decoder String
parseLevelFromNum =
    let
        toLevel courseNumber =
            toString (firstDigit courseNumber * 100)
    in
        Json.Decode.map toLevel int


firstDigit : Int -> Int
firstDigit num =
    case num // 10 of
        0 ->
            num

        x ->
            firstDigit x


{-| Decodes a field that is either the literal string "None" or a value
parsed by the given decoder.
 -}
noneOr : Decoder a -> Decoder (Maybe a)
noneOr decoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\_ -> Nothing) (parseConstant string "None")
        , Json.Decode.map Just decoder
        ]

{-| Either "None" for False, or 1 for True.
 -}
booleanish : Decoder Bool
booleanish =
    Json.Decode.oneOf
        [ Json.Decode.map (\x -> False) (parseConstant string "None")
        , Json.Decode.map (\x -> True) (parseConstant int 1)
        ]

{-| Reads a meeting time from a string like "MWF 9:30-10:20".
 -}
parseMeetingTime : Decoder MeetingTime
parseMeetingTime =
    let
        daySymbols =
            [ ( 'M', Monday )
            , ( 'T', Tuesday )
            , ( 'W', Wednesday )
            , ( 'R', Thursday )
            , ( 'F', Friday )
            ]

        toDay chars char day =
            if List.member char chars then
                Just day
            else
                Nothing

        extractDays str =
            List.filterMap (uncurry (toDay (String.toList str))) daySymbols

        toMeetingTime str =
            MeetingTime (extractDays str) str
    in
        Json.Decode.map toMeetingTime string
