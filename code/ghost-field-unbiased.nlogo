extensions [ rnd ] ;for weighted random value

breed [researchers researcher]
breed [topics topic]

researchers-own [
  hypothesis
  result
  memory ;nb! Positive result = 1, negative = 0. Start studying a topic with zero negative results (i.e., all 1s)
  continue
  published ;indicator of if the last study result was successfully published
  bias
]
topics-own [
  ID
  evidence
  ES
]



globals [
   memory-probs
]

to setup
  clear-all

  create-researchers num-researchers
  ask researchers [
    ;set hypothesis one-of range num-hyps ;originaly randomly assing researchers to hyps. But this leads to problems when some hyp gets zero
    set hypothesis item (who mod num-hyps) range num-hyps ;assign equally, see https://stackoverflow.com/questions/50541934/netlogo-assign-turtles-randomly-but-equally-to-different-groups
    set memory n-values length-memory [1] ;start with zero negatives.
    set continue 1 ;by default continue on the same topic
    if (random-float 1 < proportion-biased-researchers) [ ;on average we have then proportion-biased researchers
      set bias 1
    ]
  ]

  create-topics num-hyps
  if (non-null-effects > num-hyps) [stop] ;cannot be more true effects than effects


  let non-null n-values non-null-effects [power] ;list of non-null effects with power = power
  if (non-null-effects = 2) [ ;hard-coded to max 2 non-null effects!
    set non-null replace-item 1 non-null power2
  ]
  let ES-list sentence (n-values (num-hyps - non-null-effects) [0.05]) non-null ;combine with null effects

  let iterator -1 ;see below. Based on answer in: from https://stackoverflow.com/questions/47696190/netlogo-how-to-assign-a-value-to-a-variable-from-an-existing-string-list
  ask topics [
    set iterator iterator + 1
    set ID item iterator (range num-hyps) ;the iterator makes it so that each item in the list is only assigned once
    set ES item iterator ES-list

    set evidence n-values 3 [0] ;1st is positive, 2nd negative, 3rd proportion

  ]


  reset-ticks
end

to go

  if stop-if-zero? [ ;If true, then stop if zero. Only useful when working interactively
  if any-abandoned? [stop] ;stop if some topic has zero researchers, only happens when power >= .95
  ]

  ask researchers [
    ; Each researcher draws a study result
    let draw random-float 1 ;each researcher draws a random value from 0 - 1
    let study-topic hypothesis
    let study-power item 0 ([ES] of topics with [ID = study-topic]); for some reason is a list despite single value, so must take item to be able to compare in next step

    ifelse random-float 1 < study-power ;If power = 80%, this gives 80% chance of sig
    [ set result 1 ] ;not negative
    [ set result 0 ] ;negative

    ;Does the researcher themselves decide to publish, or put their result in the file drawer?
    let file-drawer-effect 0 ;for significant results, no file-drawer effect
    if (result = 0) [ ;for non-sig
      set file-drawer-effect researcher-publication-bias
    ]

    ifelse random-float 1 > file-drawer-effect
    [set published 1]
    [set published 0]


    ; Does the researcher p-hack?
    if (result = 0 and bias = 1) [ ;these researchers do
      if random-float 1 < p-hacking-success [ ;only equal to 0.3 so far
        set result 1
      ]
    ]

    ;set first memory value to result, push the remainder forward and drop last
    set memory sentence result memory ;cbind
    set memory remove-item 5 memory ;drop the last item, netlogo counts from 0

    ;Does the researcher successfully publish their study given journal-level publication bias?
    if (published = 1) [ ;Only if the researcher tries to publish their study (i.e., did not put in file-drawer already)

      ;determine level of journal bias
      let journal-bias 0 ;for significant effects
      if (result = 0) [ ;for non-sig
        set journal-bias journal-publication-bias
      ]
      ; try to publish
      ifelse random-float 1 > journal-bias
      [set published 1]
      [set published 0]
    ]



    ;study same topic? Probabilities based on memory with yes/no in the end
    if (memory-curve = "vote-counter")[
      set memory-probs (list 1 0.95 0.8 0.5 0.2 0)
    ]
    if (memory-curve = "reluctant")[
      set memory-probs (list 1 0.99 0.95 0.5 0.2 0)
    ]
    if (memory-curve = "rational")[
      set memory-probs rational-memory study-power
    ]

    let num-non-sig 5 - sum memory

    ;troubleshooting
    if (troubleshoot-memory-probs?) [
     print memory-probs
    ]

    ; Each researcher decides whether to continue based on their most recent and historical results
    if num-non-sig = 0 [ifelse random-float 1 < (item 0 memory-probs)
      [set continue 1]
      [set continue 0]
    ]
    if num-non-sig = 1 [ifelse random-float 1 < (item 1 memory-probs)
      [set continue 1]
      [set continue 0]
    ]
    if num-non-sig = 2 [ifelse random-float 1 < (item 2 memory-probs)
      [set continue 1]
      [set continue 0]
    ]
    if num-non-sig = 3 [ifelse random-float 1 < (item 3 memory-probs)
      [set continue 1]
      [set continue 0]
    ]
    if num-non-sig = 4 [ifelse random-float 1 < (item 4 memory-probs)
      [set continue 1]
      [set continue 0]
    ]

    if num-non-sig = 5 [ifelse random-float 1 < (item 5 memory-probs)
      [set continue 1]
      [set continue 0]
    ]
  ] ;end of ask researchers

  ; next, update evidence on topics that can be used to select topic (i.e., literature)
  ;Here publication bias plays in. For updating literature we only count researchers with published = 1
  foreach range num-hyps [hyp -> ;for each topic/hypothesis
    let n_sig found-sig hyp ;count number of significant this round
    let number-of-studies count researchers with [hypothesis = hyp and published = 1] ;count how many people published their research on the topic
    let non_sig number-of-studies - n_sig ;compute non-sig

    ask topics with [ID = hyp] [ ;update evidence in literature on each topic
      set evidence replace-item 0 evidence (item 0 evidence + n_sig) ;old sig + new sig studies total
      set evidence replace-item 1 evidence (item 1 evidence + non_sig) ;old non-sig + new non-sig studies total
      ifelse (item 0 evidence + item 1 evidence) = 0
      [set evidence replace-item 2 evidence 0]
      [set evidence replace-item 2 evidence (item 0 evidence / (item 0 evidence + item 1 evidence))]
       ;proportion significant studies for each topic
    ]
  ]

  ask researchers with [continue = 0] [;ask researchers who want a new topic to pick based on evidence

    ;remove the hypothesis they just studied
    let new_topics topics with [ID != [hypothesis] of researchers]

    ; compute total sum of proportions across topics
    let topic_proportions map [x -> item 2 x] ([evidence] of new_topics)
    ; https://stackoverflow.com/questions/72320334/how-to-use-a-command-all-the-4th-item-nested-lists-in-netlogo
    let total sum topic_proportions

    if(total > 0) [ ; add because first round all topics are at zero and then we divide by zero below
                    ; compute normalized probability for each topic
      let normalized_probabilities map [i -> i / total] topic_proportions

      ; researchers set new topic with prob equal to above
      set hypothesis pick_new_topic normalized_probabilities

      ;must also reset the memory to zero negative results
      set memory n-values 5 [1]
    ]
  ]


  tick
end

to-report found-sig [hyp]

   report sum [result] of researchers with [hypothesis = hyp and published = 1] ;count first list item of..
  ; To check if working, change one researchers result to 1 and run > [result] of researchers with [hypothesis = 1]
end

to-report pick_new_topic [normalized_probs]
  ;https://stackoverflow.com/questions/41901313/netlogo-assign-variable-using-probabilities
  let options_prob (map list (range num-hyps) normalized_probs)
  report first rnd:weighted-one-of-list options_prob last ;outputs the new hyp value
end

to-report any-abandoned?
  let num-adherents n-values num-hyps [1] ;random value
  foreach range num-hyps [hyp ->
    set num-adherents replace-item hyp num-adherents (count researchers with [hypothesis = hyp])
  ]
  report min num-adherents = 0
end

to-report rational-memory [study-power]
  let k range 6 ;0 1 2 3 4 5 significant results
  let beta 1 - study-power
  let n 5
  let alpha 0.05
  let rational-probs n-values 6 [1]

  foreach k [k_i ->
    let numerator (study-power ^ k_i) * (beta ^(n - k_i))
    let denominator numerator + (alpha ^ k_i) * ((1 - alpha) ^ (n - k_i))
    let p_i numerator / denominator
    set rational-probs replace-item k_i rational-probs p_i
  ]

  report reverse rational-probs ;reverse, because rational-probs is a list for sig. and we want a list of probs for non-sig

end
;to assign-topic-id ;from https://stackoverflow.com/questions/47696190/netlogo-how-to-assign-a-value-to-a-variable-from-an-existing-string-list
;  let iterator -1
;  ask topics ;NB. ask goes in random order. Iteration will only go to number of topics so won't overshoot
;  [ set iterator iterator + 1
;    set ID item iterator (range num-hyps)
;  ]
;end ;use when setting up ID for topics

; this below is old
;  let sig count researchers with [result = 1]
;  let non-sig count researchers with [result = 0]
;  let total sig + non-sig
;  set prop-sig (sig / total)
@#$#@#$#@
GRAPHICS-WINDOW
460
620
633
794
-1
-1
5.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
34
21
97
54
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
113
21
176
54
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
25
85
197
118
num-researchers
num-researchers
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
205
85
377
118
num-hyps
num-hyps
1
3
3.0
1
1
NIL
HORIZONTAL

SLIDER
205
130
377
163
length-memory
length-memory
0
10
5.0
1
1
NIL
HORIZONTAL

PLOT
370
320
635
560
Prop. sig
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"pen-0" 1.0 0 -7500403 true "" "ask topics [\n\n create-temporary-plot-pen (word who)\n set-plot-pen-color ID * 10 + 5\n plotxy ticks (item 2 evidence)\n]"
"prop = 0.05" 1.0 0 -16777216 true "" "plotxy ticks 0.05"
"prop = power" 1.0 0 -16777216 true "" "plotxy ticks power"
"prop = power2" 1.0 0 -16777216 true "" "plotxy ticks power2"

PLOT
15
320
360
560
Researcher per topic
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"pen-0" 1.0 0 -1 true "" "ask topics [\nlet hyp ID\n let res count (researchers with [hypothesis = hyp])\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color hyp * 10 + 5\n  plotxy ticks res\n\n\n]\n\n"
"pen-1" 1.0 0 -16777216 true "" "plotxy ticks (num-researchers / num-hyps)"

BUTTON
200
20
263
53
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
25
130
197
163
non-null-effects
non-null-effects
0
2
2.0
1
1
NIL
HORIZONTAL

SLIDER
25
180
197
213
power
power
0
1
0.3
0.05
1
NIL
HORIZONTAL

SWITCH
280
20
402
53
stop-if-zero?
stop-if-zero?
1
1
-1000

SLIDER
205
180
377
213
power2
power2
0
1
0.5
0.05
1
NIL
HORIZONTAL

CHOOSER
410
85
548
130
memory-curve
memory-curve
"vote-counter" "reluctant" "rational"
0

SWITCH
415
20
620
53
troubleshoot-memory-probs?
troubleshoot-memory-probs?
1
1
-1000

SLIDER
25
230
197
263
journal-publication-bias
journal-publication-bias
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
205
230
380
263
researcher-publication-bias
researcher-publication-bias
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
400
230
622
263
proportion-biased-researchers
proportion-biased-researchers
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
445
150
617
183
p-hacking-success
p-hacking-success
0
1
0.1
0.05
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="3-hyps-one-non-null" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3-hyps-2-non-null" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2-hyps-1-null" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3-hyps-2-varying-non-null-vote-counter" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power2">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;vote-counter&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="journal-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2-hyps-1-null-rational" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;rational&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3-hyps-2-varying-non-null-rational" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power2">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;rational&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="publication-bias-30-power" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="researcher-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="troubleshoot-memory-probs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="journal-publication-bias">
      <value value="0"/>
      <value value="0.5"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;vote-counter&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power2">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="p-hacking-0.3-0.5-power-var-prop-biased-researchers" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="researcher-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="journal-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;vote-counter&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="troubleshoot-memory-probs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-biased-researchers">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power2">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-hacking-success">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="p-hacking-0.3-0.5-power-all-biased" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>count researchers with [hypothesis = 0]</metric>
    <metric>count researchers with [hypothesis = 1]</metric>
    <metric>count researchers with [hypothesis = 2]</metric>
    <enumeratedValueSet variable="researcher-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="journal-publication-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-researchers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-curve">
      <value value="&quot;vote-counter&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="troubleshoot-memory-probs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-if-zero?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-biased-researchers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power2">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hyps">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-hacking-success">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length-memory">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="non-null-effects">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
