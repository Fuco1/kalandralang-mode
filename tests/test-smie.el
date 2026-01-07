;; -*- lexical-binding: t -*-

(require 'kalandralang-mode-test-helpers)

(describe "SMIE"

  (describe "repeat"

    (it "with block"
      (expect "repeat {
foo
bar
} until baz" :to-indent-as
"repeat {
  foo
  bar
} until baz"))

    (it "with single expression"
      (expect "repeat
foo
until baz" :to-indent-as
"repeat
  foo
until baz")))

  (describe "until"

    (it "with single expression"
      (expect "until
tier \"Dexterity\" +
tier \"Intelligence\" +
tier \"Strength\" <= 6
do chaos
" :to-indent-as "until
  tier \"Dexterity\" +
  tier \"Intelligence\" +
  tier \"Strength\" <= 6
do chaos
"))

    (it "with parens around the condition"
      (expect "until (
tier \"Dexterity\" +
tier \"Intelligence\" +
tier \"Strength\" <= 6
) do chaos
" :to-indent-as "until (
  tier \"Dexterity\" +
  tier \"Intelligence\" +
  tier \"Strength\" <= 6
) do chaos
"
)
      )
    )

  (describe "buy"

    (it "with with statements"
      (expect "
buy exact \"foo\"
with fractured \"foo\"
with \"bare\"
with \"kek\"

exalt
" :to-indent-as
"
buy exact \"foo\"
  with fractured \"foo\"
  with \"bare\"
  with \"kek\"

exalt
")
      )
    ))
