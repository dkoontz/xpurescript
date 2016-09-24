module Test.Main where

import Prelude
import Bob (talkTo)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.State (StateT)
import Node.Process (PROCESS)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

questionResponse :: String
questionResponse = "Sure."

yellResponse :: String
yellResponse = "Whoa, chill out!"

silenceResponse :: String
silenceResponse = "Fine. Be that way!"

defaultResponse :: String
defaultResponse = "Whatever."

main :: forall e. Eff ( process :: PROCESS, console :: CONSOLE | e) Unit
main = run [consoleReporter] do
  questionSpecs
  yellSpecs
  silenceSpecs
  defaultSpecs

questionSpecs :: forall e. StateT (Array Group) (Aff e) Unit
questionSpecs =
  describe "Bob answers 'Sure.' if you ask him a question." do
    it "Does this cryogenic chamber make me look fat?" $
      talkTo "Hello?" `shouldEqual` questionResponse

    it "asking a numeric question" $
      talkTo "You are, what, like 15?" `shouldEqual` questionResponse

    it "asking gibberish" $
      talkTo "fffbbcbeab?" `shouldEqual` questionResponse

    it "question with only numbers" $
      talkTo "4?" `shouldEqual` questionResponse

    it "non-letters with question" $
      talkTo ":) ?" `shouldEqual` questionResponse

    it "prattling on" $
      talkTo "Wait! Hang on. Are you going to be OK?" `shouldEqual` questionResponse

    it "ending with whitespace" $
      talkTo "Okay if like my  spacebar  quite a bit?   " `shouldEqual` questionResponse

yellSpecs :: forall e. StateT (Array Group) (Aff e) Unit
yellSpecs =
  describe "He answers 'Whoa, chill out!' if you yell at him." do
    it "shouting" $
      talkTo "WATCH OUT!" `shouldEqual` yellResponse

    it "shouting gibberish" $
      talkTo "FCECDFCAAB" `shouldEqual` yellResponse

    it "forceful question" $
      talkTo "WHAT THE HELL WERE YOU THINKING?" `shouldEqual` yellResponse

    it "shouting numbers" $
      talkTo "1, 2, 3 GO!" `shouldEqual` yellResponse

    it "shouting with special characters" $
      talkTo "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!" `shouldEqual` yellResponse


    it "shouting with umlauts" $
      talkTo "ÜMLÄÜTS!" `shouldEqual` yellResponse

    it "shouting with no exclamation mark" $
      talkTo "I HATE YOU" `shouldEqual` yellResponse

silenceSpecs :: forall e. StateT (Array Group) (Aff e) Unit
silenceSpecs =
  describe "He says 'Fine. Be that way!' if you address him without actually saying anything." do
    it "silence" $
      talkTo "" `shouldEqual` silenceResponse

    it "prolonged silence" $
      talkTo "          " `shouldEqual` silenceResponse

    it "alternate silence" $
      talkTo "\t\t\t\t\t\t\t\t\t\t" `shouldEqual` silenceResponse

    -- This fails due to parsing unicode literal, TODO: figure out how to put these into strings
    -- it "other whitespace" $
    --   talkTo "\n\r \t\u000b\u00a0\u2002" `shouldEqual` silenceResponse

defaultSpecs :: forall e. StateT (Array Group) (Aff e) Unit
defaultSpecs =
  describe "He answers 'Whatever.' to anything else." do
    it "stating something" $
      talkTo "Tom-ay-to, tom-aaaah-to." `shouldEqual` defaultResponse

    it "using acronyms in regular speech" $
      talkTo "It's OK if you don't want to go to the DMV." `shouldEqual` defaultResponse

    it "talking forcefully" $
      talkTo "Let's go make out behind the gym!" `shouldEqual` defaultResponse

    it "only numbers" $
      talkTo "1, 2, 3" `shouldEqual` defaultResponse

    it "calmly speaking with umlauts" $
      talkTo "ÜMLäÜTS!" `shouldEqual` defaultResponse

    it "statement containing question mark" $
      talkTo "Ending with ? means a question." `shouldEqual` defaultResponse

    it "multiple line question" $
      talkTo "\nDoes this cryogenic chamber make me look fat?\nno" `shouldEqual` defaultResponse

    it "starting with whitespace" $
      talkTo "         hmmmmmmm..." `shouldEqual` defaultResponse

    it "non-question ending with whitespace" $
      talkTo "This is a statement ending with whitespace      " `shouldEqual` defaultResponse
