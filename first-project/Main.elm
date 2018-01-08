module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Quiz = { text : String
                  , answers : List String
                  , rightAnswer : String
                  }

type alias Answer = { answer : String
                    , quiz : Quiz
                    , right : Bool
                    }

type Page
    = WelcomePage
    | QuizPage
    | CongratulationsPage
    | LoserPage


type alias Model = { remaining : Int
                   , quizes : List Quiz
                   , page : Page
                   , userAnswers : List Answer
                   }

initialModel : Model
initialModel = { remaining = 0
               , quizes =
                     [ { text = "Qual a capital de Alagoas?"
                       , answers = ["Natal", "Maceió", "Campo Grande"]
                       , rightAnswer = "Maceió"
                       }
                     , { text = "Qual o ano do descobrimento do Brasil?"
                       , answers = ["1520", "1502", "1500"]
                       , rightAnswer = "1500"
                       }
                     , { text = "Qual o elemento mais eletronegativo da tabela periódica"
                       , answers = ["Flúor", "Chumbo", "Hidrogênio"]
                       , rightAnswer = "Flúor"
                       }
                     ]
               , page = WelcomePage
               , userAnswers = [] }


init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)


-- UPDATE

type Msg
    = Tick Time
    | StartLevel Int
    | Reply Quiz String


isSameQuiz : Quiz -> Quiz -> Bool
isSameQuiz quiz1 quiz2 =
    quiz1.text == quiz2.text


getAnswerQuiz : Answer -> Quiz
getAnswerQuiz answer = answer.quiz


updateAnswers : List Answer -> String -> Quiz -> List Answer
updateAnswers answers answerText quiz =
    let
        answer = { answer = answerText
                 , quiz = quiz
                 , right = quiz.rightAnswer == answerText
                 }
    in
        answer :: List.filter (not << isSameQuiz quiz << getAnswerQuiz) answers


getAnswerByQuiz : List Answer -> Quiz -> Maybe Answer
getAnswerByQuiz answers quiz =
    List.head <| List.filter (isSameQuiz quiz << getAnswerQuiz) answers


isAllAnswersRight : List Answer -> Bool
isAllAnswersRight answers =
    List.all (\answer -> answer.right) answers


isQuizRight : List Answer -> List Quiz -> Bool
isQuizRight answers quizes =
    List.length answers == List.length quizes && isAllAnswersRight answers


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        let
            remaining = model.remaining - 1
        in if remaining < 0 && model.page == QuizPage then
               ({ model | page = LoserPage }, Cmd.none)
           else
               ({ model | remaining = remaining}, Cmd.none)
    StartLevel level ->
        case model.quizes of
            quiz::quizes ->
                ({ model | page = QuizPage , remaining = 30 // level }, Cmd.none)
            [] ->
                (model, Cmd.none)
    Reply quiz answer ->
        let
            answers = updateAnswers model.userAnswers answer quiz
        in
            if isQuizRight answers model.quizes then
                ({ model | page = CongratulationsPage }, Cmd.none)
            else
                ({ model | userAnswers = answers }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW

welcomeView : Html Msg
welcomeView =
    div []
        [ div []
              [ h2 [] [ text "Clique no nível abaixo para começar" ]
              ]
        , div []
              [ button [ onClick <| StartLevel 1 ] [ text "Fácil" ]
              , button [ onClick <| StartLevel 2 ] [ text "Médio" ]
              , button [ onClick <| StartLevel 3 ] [ text "Difícil" ]
              ]
        ]


remainingView : Int -> Html Msg
remainingView remaining =
    div [] [ text "Você tem "
           , span
                 (
                  if remaining < 10 then
                      [ style [ ("color", "#f33") ] ]
                  else
                      []
                 )
                 [text <| toString remaining]
           , text " segundos para responder todas as perguntas" ]



answerView : Quiz -> String -> Html Msg
answerView quiz answer =
    li [] [ input [ type_ "radio", name quiz.text , onClick <| Reply quiz answer ] []
          , text answer ]


answersView : Quiz -> Html Msg
answersView quiz =
    ul [] <|
        List.map (answerView quiz) quiz.answers


quizView : Maybe Answer -> Quiz -> Html Msg
quizView hasAnswer quiz =
    div [] <|
        [ h3 [] [ text quiz.text ]
        , answersView quiz
        ] ++
        case hasAnswer of
            Just answer ->
                if answer.right then
                    [ strong [ style [("color", "green")] ] [text "acerto mizeravi" ]]
                else
                    [ strong [ style [("color", "red")]] [text "errrou!"]]
            Nothing ->
                []


quizesView : Model -> Html Msg
quizesView model =
    div []
        [ h1 [] [ text "Responda as perguntas abaixo" ]
        , remainingView model.remaining
        , div [] <|
            List.map (\quiz -> quizView (getAnswerByQuiz model.userAnswers quiz) quiz) <|
                model.quizes
        ]


congratulationsView : Html Msg
congratulationsView =
    img [ style [ ("margin-top", "10px")
                , ("width", "100%") ]
        , src "http://78.media.tumblr.com/691319250ea56161aeddcdf7ac3ae0e6/tumblr_miazecVM8q1r3ifxzo1_500.gif" ] []


loserView : Html Msg
loserView =
    img [ style [ ("margin-top", "10px")
                , ("width", "100%") ]
        , src "http://4.bp.blogspot.com/-ChRR74BycAU/Um5uyFvQnhI/AAAAAAAAALk/3sZIVkZsVG4/s1600/gta_sa+2013-10-27+18-10-17-752.png" ] []


layout : Html Msg -> Html Msg
layout body =
    div [ style [ ("margin", "10px auto")
                , ("font-family", "sans")
                , ("width", "800px")
                ]
        ]
        [ div [ style [ ("background", "#010101")
                      , ("color", "#fafafa")
                      , ("padding", "10px")
                      , ("border-radius", "10px" )
                      ]
              ]
              [ h1 [] [ text "Bem-vindo ao super quiz!" ] ]
        , body ]

view : Model -> Html Msg
view model =
    layout <|
        case model.page of
            QuizPage ->
                quizesView model
            WelcomePage ->
                welcomeView
            CongratulationsPage ->
                congratulationsView
            LoserPage ->
                loserView
