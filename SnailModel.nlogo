breed [snails snail] ;; set snail up as a breed of turtle

undirected-link-breed [friends friend] ;; sets up link for tracking non-unique contacts

undirected-link-breed [uniquefriends uniquefriend] ;; sets up link for tracking unique contacts

snails-own [
  contact-count ;; snails keep track of contacts
  unique-contact-count ;; snails keep track of unique contacts
  last-meal ;; snails keep track of the last time they ate
  patches-visited ;; a list of the unique patches a snail has visited
  last-patch ;; the last patch the snail was on
  patch-count ;; the number of times the snail has changed patches
  patch-time ;; the average time a snail stays on a patch ;;{Does this need to exist? Just divide ticks by patch-count as needed?}
  edge-time ;; time snails spend on the edge of the environment
  edge-percent ;; percent time snails spend on the edge of the environment ;;{Does this need to exist? Just divide edge-time by ticks?}
  sm ;; just a random number to compare against stochasticity value to determine if snail will move randomly
  sick?        ;; if true, the turtle is infectious
  recovered?   ;  infected but then recovered, susceptible again?
  sick-timer ; just a counter to keep track of exposed but not yet infectious or recovered timing
]

patches-own [
  coverage ;; resource density on a patch, like algal percent cover
]

friends-own [
  friendship-duration ;; the duration of the contact
]

globals [
  snail-size ;; size of snails
  aggregation-index ;; measures aggregation of the resources
  contact-durations ;; list that holds all the durations of the contacts
  avg-contact-duration ;; calculation of the average duration of a contact  ;;{Does this need to exist? Just divide contact-durations by total #?}

  ;; variables on the interface not initialized in code
  ;feeding-rate              ;; the feeding rate of the snails
  ;number-of-snails          ; number of initial consumers
  ;movement-mode             ; random movement or GUD based
       ; 99 = random, 1 = GUD-based movement
  ;stochasticity             ; do consumers make mistakes in GUD?
  ;GUD                       ; giving up density as a percent, 0-100
  ;algae-distribution
       ; 99 = random, 0-6 equal 0/6, 1/6, 2/6, of old...
  ;initial-algae-coverage    ; resource density as a percent, 0-100
  ;starting-infections       ; number of initial infections among consumers
  ;infectiousness            ; what proportion of contacts between infected and susceptible lead to infection? 0 to 1
  ;recover-time              ; length of time infected consumer remains infectious
]

to setup

  clear-links
  clear-all
  reset-ticks

  ;; initialize consumer traits
  ;set feeding-rate 5
  set snail-size 5

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Set up resource patches
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask patches [set pcolor brown]

  ;; Are resource patches randomly distributed in the world? Then do this...

   if (algae-distribution = 99) [
    let random-tiles (list (random 144))
    while [length random-tiles < 64] [
      set random-tiles (lput (random 144) random-tiles)
      set random-tiles (remove-duplicates random-tiles)
    ]
    foreach random-tiles [ ?1 ->
      let tile-xcor (?1 mod 12)
      let tile-ycor (floor (?1 / 12))
      ask patches with [pxcor = tile-xcor and pycor = tile-ycor] [ set pcolor lime ]
    ]
  ]

  ;; Are resource patches determined by aggregation index/experimental procedure? Then do this....

  if (algae-distribution = 0) [
    ask patches with [pxcor = 4 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 11] [ set pcolor lime ]

  ]

  if (algae-distribution = 1) [
    ask patches with [pxcor = 0 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 7] [ set pcolor lime ]
  ]

  if (algae-distribution = 2) [
    ask patches with [pxcor = 0 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 7] [ set pcolor lime ]

  ]

  if (algae-distribution = 3) [
    ask patches with [pxcor = 0 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 9] [ set pcolor lime ]
  ]

  if (algae-distribution = 4) [
    ask patches with [pxcor = 0 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 11] [ set pcolor lime ]
  ]

  if (algae-distribution = 5) [
    ask patches with [pxcor = 0 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 1] [ set pcolor lime ]

    ask patches with [pxcor = 0 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 3] [ set pcolor lime ]

    ask patches with [pxcor = 2 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 1] [ set pcolor lime ]

    ask patches with [pxcor = 2 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 3] [ set pcolor lime ]

    ask patches with [pxcor = 2 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 5] [ set pcolor lime ]

    ask patches with [pxcor = 2 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 7] [ set pcolor lime ]

    ask patches with [pxcor = 4 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 5] [ set pcolor lime ]

    ask patches with [pxcor = 4 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 7] [ set pcolor lime ]

    ask patches with [pxcor = 6 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 5] [ set pcolor lime ]

    ask patches with [pxcor = 6 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 7] [ set pcolor lime ]

    ask patches with [pxcor = 8 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 5] [ set pcolor lime ]

    ask patches with [pxcor = 8 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 7] [ set pcolor lime ]

    ask patches with [pxcor = 8 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 9] [ set pcolor lime ]

    ask patches with [pxcor = 8 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 8 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 9 and pycor = 11] [ set pcolor lime ]

    ask patches with [pxcor = 10 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 8] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 9] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 9] [ set pcolor lime ]

    ask patches with [pxcor = 10 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 10] [ set pcolor lime ]
    ask patches with [pxcor = 10 and pycor = 11] [ set pcolor lime ]
    ask patches with [pxcor = 11 and pycor = 11] [ set pcolor lime ]

  ]

  if (algae-distribution = 6) [
    ask patches with [pxcor = 0 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 0 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 1 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 2 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 3 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 4 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 5 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 6 and pycor = 7] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 0] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 1] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 2] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 3] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 4] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 5] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 6] [ set pcolor lime ]
    ask patches with [pxcor = 7 and pycor = 7] [ set pcolor lime ]
  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this block calculates an aggregation index by asking algae tiles if there are algae tiles next to them
  ;; the logic is to ask each patch if they are algae covered, and if so, ask if there are algae patches on their four sizes.
  ;;  because of the size of the world and the tiles, there are 24
  ;; opportunities for unique neighbors to be on different tiles, and the # out of 24 defines the aggregration index.
  ;; The code turns
  ;; patches from lime to green as it goes, so it only counts unique neighbor combinations. clever. Easiest to understand in the 6/6 aggregation
  ;; situation, where all the resources are clumped in the bottom left.  first algae patch has one patch to the right and one below with algae.
  ;; the next two algae tiles across have the same. The last patch across the top
  ;; row has only one, the one on its bottom edge.  2 2 2 1. This pattern repeats for the next two rows. 2 2 2 1. Then the last row has only
  ;; new neighbor patches
  ;; on different tiles to its right... 1 1 1 0. 24 total. the maximum aggregation has 24/24. the minimum aggregation has 0/24. wow.

  let algaefriends 0

  ask patches [
    if pcolor = lime [
      let this-tile-pxcor pxcor
      let this-tile-pycor pycor
      set algaefriends algaefriends + (count neighbors4 with [ pcolor = lime and (pxcor != this-tile-pxcor or pycor != this-tile-pycor)])
      set pcolor green
    ]
  ]

  set aggregation-index algaefriends / 24
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set correct initial resource densities and color-code the patches
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  set contact-durations [] ;; initialize list
  set avg-contact-duration 0

  ask patches [ ;; check if a patch has algae on it, and sets the biomass to what was previously calculated if so
    if-else pcolor = green
    [ set coverage initial-algae-coverage ]
    [ set coverage 0 ]
    if (coverage < GUD and coverage > 0) [ set pcolor 126 ] ;;set "alarm" color to indicate starting coverage is less than GUD
  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; create consumers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  set-default-shape snails "snail"

  create-snails number-of-snails ;;  settings for the snails
  [
    setxy random-xcor random-ycor ;; random placement
    move-to patch-here
    set color red
    set size snail-size / 10
    set contact-count 0
    set unique-contact-count 0
    set patches-visited (list (list [pxcor] of patch-here [pycor] of patch-here))
    set last-patch patch-here
    set patch-count 1
    set patch-time (ticks / patch-count)
    set edge-time 0
    set edge-percent 0
    set last-meal 0
    set sick? false       ;; if true, the turtle is infectious
    ;set exposed? false    ;;infected but not infectious
    set recovered? false
    set sick-timer 0
    set label who

  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;start with some infection
  let sickos (n-of starting-infections snails)
  ask sickos [
    set sick? true
    set color black]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Order of procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
 if ticks < 1
  [correct-start
  tick]
  move-snails
  count-all-snail-contacts
  count-unique-snail-contacts
  snails-eat
  infect-snails
  tick
  calculate-patches-visited
  calculate-patch-time
  calculate-edge-time
 ; if ticks >= sim-duration [stop]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  infection procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to infect-snails

  ask snails [
    if not sick? AND not recovered? [
     if any? turtles-here with [sick? = true]
      [ if random 101 <= infectiousness ;if random number is less than or equal to infectionness unifected snail becomes infected
        [set sick? true
          set color black
        set sick-timer 0]
      ]
    ]

  ]

  ask snails with [sick? = true] [
    set sick-timer sick-timer + 1
    if sick-timer > recover-time
    [set sick? false
      set recovered? true
      set color blue
      ;set sick-timer 0
    ]
  ]

  ;show [who] of turtles with [sick? = true] ; just debugging code

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  consumer movement procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to move-snails ;; calls the appropriate movement mode for the snails based on the settings

  ask snails [
     ifelse (movement-mode = 99)
     [ move-snails-random]

     [set sm random 101  ;generates a random integer between 0 and 100
      ifelse sm < stochasticity ;if random number is less than stoch snail moves random, else snail moves optiaml foraging
      [ask self [ move-snails-random]] ; TRUE random number less than stochcasticty level
      [ask self [ move-snails-optimal-foraging]]
     ]
  ]

end

;to move-snails-random ;; alternate procedure for moving the snails randomly

 ; move-to one-of neighbors

;end


;to move-snails-optimal-foraging ;; moves the snails simulating optimal foraging

 ;   ifelse ([coverage] of patch-here > (GUD))
  ;  [stop]
    ;[
          ;move-to patch-here  ;; go to patch center
         ; let p (max-one-of neighbors [coverage])  ;; or neighbors4
         ; ifelse [coverage] of p >= [coverage] of patch-here
         ; [face p
         ;  move-to p
         ; ]
   ;       [move-to one-of neighbors]
   ; ]

;end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Rule of Two Movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to correct-start
  ask snails[
  if(count snails-here >= 3)
    [move-to min-one-of (patches with [not any? snails-here]) [distance myself]]
  ]
end

to move-snails-random
  ifelse(patch-ahead 1 != nobody) ;if someone ahead
  [ifelse(count snails-on patch-ahead 1  >= 2) ;two or more snails ahead -- change to 2 for rule of two
  [rt random 360] ; right turn
  [fd 1
    rt random 360]; else move forward 1 and random turn
  ]
  [fd 1
    rt random 360]
end



to move-snails-optimal-foraging ;; moves the snails simulating optimal foraging
    ifelse ([coverage] of patch-here > (GUD))
    [stop]
    ;[
          ;move-to patch-here  ;; go to patch center
         ; let p (max-one-of neighbors [coverage])  ;; or neighbors4
         ; ifelse [coverage] of p >= [coverage] of patch-here
         ; [face p
         ;  move-to p
         ; ]
  [ifelse(patch-ahead 1 != nobody)
  [ifelse(count snails-on patch-ahead 1  >= 2) ;two or more snails ahead -- change to 2 for rule of two
  [rt random 360]
  [fd 1
    rt random 360]
  ]
  [fd 1
      rt random 360]
  ]
    ;]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  resource consumption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to snails-eat ;; snails reduce the biomass of the patch they are on

  ask patches [
    set coverage coverage - (feeding-rate * (count snails-here))
    if (coverage <= GUD and coverage > 0) [ set pcolor 126 ]
    if (coverage <= 0) [
      set coverage 0
      set pcolor brown
    ]
  ]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping track of contacts, unqiue contacts, and contact durations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NUMBER OF CONTACTS IS BEING DOUBLE COUNTED (A-B and B-A ARE COUNTED) NEED TO DVIDE THE OUTPUT BY 2
;; AVERAGE CONTACT DURATION OUTPUT IS CORRECT


; Each snail has list of duration and then average per snail
to count-all-snail-contacts ;; counts non-unique contacts between snails

  ask snails [ ;; for each snail, count all snails it is in contact with
    let firstsnail self ;; set a pointer to the snail that contacts are being counted for
    let firstsnailnewfriends 0 ;; initialize counter for the contacts
    ask other turtles-here [ ;; iterate through the first snail's neighbors
      if (not friend-neighbor? firstsnail) [ ;; if there is not a link between them, create one and count the contact
        create-friend-with firstsnail
        set contact-count (contact-count + 1)
        set firstsnailnewfriends (firstsnailnewfriends + 1)
      ]
    ]
    set contact-count (contact-count + firstsnailnewfriends) ;; add the count of new links to the contact count
  ]
  ask friends with [link-length >= 1] [ ;; break links between snails that have moved apart
    ;; friendship-duration is the current contact duration and contact duration is the list
    set contact-durations (lput friendship-duration contact-durations) ;; input the contact information into the list
    die ;; break the link
  ]
  ask friends [ ;; ask the link if it is still there
    set friendship-duration (friendship-duration + 1) ;; then add one to the duration
  ]
  if-else ((length contact-durations + count friends) = 0) ; does this snail have any previous contact durations or current contacts
  [ set avg-contact-duration 0 ] ;; if not set to zero
  [ set avg-contact-duration ((sum contact-durations + sum [friendship-duration] of friends) / (length contact-durations + count friends)) ]
;; contact-durations is all previous contacts -- friendship durations is current contacts  / previous + current contacts
;; because contact durations is only updated when current contact is over
;; only asking friends so contact durations is calculated off of the links -- therefore contact duration is correct
end

to count-unique-snail-contacts ;; sets new links to count unique contacts between snails
  ask snails [ create-uniquefriends-with other turtles-here ]
  ask snails [ set unique-contact-count (count uniquefriend-neighbors) ]
  ;; total possible unique contacts is n(n-1)/2, where n is number of snails
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping track of patches visited, time on a patch, resource levels, and time on edges of the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calculate-patches-visited ;; maintain the list of patches the snail has visited
  ask snails [
    set patches-visited (lput (list [pxcor] of patch-here [pycor] of patch-here) patches-visited)
    set patches-visited (remove-duplicates patches-visited)
  ]
end

to calculate-patch-time ;; calculates the average time the snails spend on each patch
  ask snails [
    if (([pxcor] of patch-here != [pxcor] of last-patch) or ([pycor] of patch-here != [pycor] of last-patch)) [
      set patch-count (patch-count + 1)
    ]
    set last-patch patch-here
    set patch-time (ticks / patch-count)
  ]
end

to calculate-edge-time ;; calculates the time the snails spend on the edge of the environment
  ask snails [
    if (([pxcor] of patch-here = 0) or ([pxcor] of patch-here = 5) or ([pycor] of patch-here = 0) or ([pycor] of patch-here = 5)) [ set edge-time (edge-time + 1) ]
    set edge-percent ((edge-time / ticks) * 100)
  ]
end

to-report resource-level
report sum [coverage] of patches / 6400
end

to-report report-edge-percent
report mean [edge-percent] of snails
end

to-report report-patch-time
report mean [patch-time] of snails
end

to-report Ss
  report count turtles with [sick? = false AND recovered? = false]
end

to-report Is
  report count turtles with [sick? = true]
end

to-report Rs
  report count turtles with [recovered? = true]
end

to-report avg-contacts
  report mean [contact-count] of snails
end


to-report total-contacts
  report sum [contact-count] of snails
end

to-report avg-unique-contacts
  report mean [unique-contact-count] of snails
end

to-report average-contact-duration
  report mean [avg-contact-duration] of snails
end

to-report report-patches-visited
report mean [length patches-visited] of snails
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
420
10
983
574
-1
-1
46.3
1
10
1
1
1
0
0
0
1
0
11
0
11
0
0
1
ticks
30.0

BUTTON
9
10
64
43
setup
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
85
10
140
43
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
70
218
103
number-of-snails
number-of-snails
0
64
64.0
1
1
NIL
HORIZONTAL

TEXTBOX
11
54
161
72
snail parameters
11
0.0
1

TEXTBOX
11
310
161
328
algae parameters
11
0.0
1

PLOT
231
12
391
132
histogram of snail contacts
NIL
NIL
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [contact-count] of snails"

BUTTON
160
10
215
43
step
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
13
272
221
305
GUD
GUD
0
100
21.8
1
1
%
HORIZONTAL

SLIDER
5
395
215
428
initial-algae-coverage
initial-algae-coverage
0
100
25.0
10
1
%
HORIZONTAL

SLIDER
13
228
221
261
stochasticity
stochasticity
0
100
100.0
10
1
%
HORIZONTAL

MONITOR
232
136
392
177
aggregation index
aggregation-index
4
1
10

MONITOR
232
181
392
222
avg contacts
mean [contact-count] of snails
2
1
10

MONITOR
234
412
394
453
avg edge time %
mean [edge-percent] of snails
2
1
10

MONITOR
233
321
394
362
avg patches visited
mean [length patches-visited] of snails
2
1
10

MONITOR
234
367
394
408
avg patch time
mean [patch-time] of snails
2
1
10

MONITOR
233
274
393
315
avg contact duration
avg-contact-duration
2
1
10

MONITOR
233
227
393
268
avg unique contacts
mean [unique-contact-count] of snails
2
1
10

SLIDER
12
473
185
506
starting-infections
starting-infections
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
12
507
185
540
infectiousness
infectiousness
0
100
80.0
1
1
NIL
HORIZONTAL

SLIDER
13
543
186
576
recover-time
recover-time
0
100
35.0
1
1
NIL
HORIZONTAL

TEXTBOX
13
451
150
469
disease parameters
11
0.0
1

MONITOR
210
458
405
503
prop. of original resources left
resource-level
2
1
11

INPUTBOX
32
109
145
169
feeding-rate
0.035
1
0
Number

INPUTBOX
15
330
165
390
algae-distribution
6.0
1
0
Number

INPUTBOX
25
170
172
230
movement-mode
1.0
1
0
Number

PLOT
20
600
220
750
SIR
ticks
Number of snails
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"I" 1.0 0 -16777216 true "" "plot count snails with [sick? = true]"
"R" 1.0 0 -13345367 true "" "plot count snails with [recovered? = true]"
"S" 1.0 0 -2674135 true "" "plot count snails with [sick? = false AND recovered? = false]"

@#$#@#$#@
## WHAT IS IT?

This is an agent-based model based on microcosm experiments conducted with an aquatic snail that links individual-level behavior with population-level processes, focusing on how foraging behaviors and resource distribution affect contact rates between individuals in a population.

## HOW IT WORKS

The agent-based model consists of patches arranged in a 12 by 12 grid, and each patch has a state variable representing the biomass of the algae on the patch. The model allows for seven different algae distributions, ranging from “0” or completely uniform to increasingly clustered to completely clustered at “6”. Agents, representing snails, move around during the agent-based model simulations and are allowed to forage completely randomly, forage optimally, or somewhere in between those two settings. Foraging behavior is controlled by stochasticity, where 0 indicates optimal foraging and 100 indicates random foraging. Optimal foraging is defined by agents staying on a resource patch until it dropped below a certain biomass and then they would randomly search for a new patch. 


## HOW TO USE IT
Before starting the simulation, parameters specific to patches and agents can be adjusted. The number of agents included in the simulation can be adjusted by using the number-of-snails slider. The snail-feeding-rate is the amount of algae a snail consumes per time step and can be adjusted by typing a new number in the text box. GUD represents giving up density or the amount of resources that are left on a tile when a snail will move off the tile when foraging optimally, and can be adjusted with the slider. Lastly, the agents’ foraging behavior can be set using the stochasticity slider. A stochasticity of 0 indicates that the agents will forage completely optimally, while a stochasticity of 100 means agents will forage completely randomly.

The patch parameters that can be adjusted include initial-algae-coverage and algae-distribution. The algae-distribution box allows the user to enter the aggregation index of 0,1,2,3,4,5, or 6. Resources begin uniformly spaced at an aggregation index of 0 and begin to cluster as aggregation index increases, until they are completely clustered at an aggregation index of 6. The initial-algae-coverage determines the total amount of algae at the beginning of the experiment, and can be adjusted with the slider.

Lastly, simple disease dynamics in the simulation can be adjusted with the disease parameters. The number of starting infected agents can be adjusted with the starting-infections slider. Additionally, how infectious the pathogen is can be increased by increasing the infectiousness slider. Lastly, the recover-time, or time that is takes for a snail to recover from the infection can be adjusted with the slider.

The model outputs the average number of contacts that agents made, the duration of those contacts, the average number of patches visited, the average patch time, the average proportion of resources left, and the average edge time.

When all parameters are set, the simulation is started by using the SETUP button to set up the agents and patches and the GO button starts the simulation


## NETLOGO FEATURES

Uses links to count both contacts and unique contacts.

## CREDITS AND REFERENCES

Zachary Gajewski, Philip McEmurray, Jeremy Wojdak, Cari McGregor, Lily Zeller, Hannah Cooper, Lisa K. Belden, Skylar Hopkins





@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

snail
true
0
Polygon -7500403 true true 45 210 195 210 225 150 195 135 135 180 45 210
Circle -1184463 true false 56 71 127
Circle -16777216 false false 78 93 85
Circle -16777216 false false 66 81 108
Circle -7500403 true true 195 105 60
Line -7500403 true 225 105 240 75
Line -7500403 true 240 120 255 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="snail_contact_avg_contacts_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_contacts_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_unique_contacts_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [unique-contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_unique_contacts_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [unique-contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_contact_duration_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>avg-contact-duration</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_contact_duration_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>avg-contact-duration</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_edge_time_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [edge-percent] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_edge_time_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [edge-percent] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_patches_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [length patches-visited] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_patches_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [length patches-visited] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_patch_time_random_aggregation" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [patch-time] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_patch_time_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>mean [patch-time] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_densities_contacts_random_aggregation" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>[contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="14"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_densities_contacts_fixed_aggregation" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="45"/>
    <metric>aggregation-index</metric>
    <metric>[contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;aggregation index 0/6&quot;"/>
      <value value="&quot;aggregation index 1/6&quot;"/>
      <value value="&quot;aggregation index 2/6&quot;"/>
      <value value="&quot;aggregation index 3/6&quot;"/>
      <value value="&quot;aggregation index 4/6&quot;"/>
      <value value="&quot;aggregation index 5/6&quot;"/>
      <value value="&quot;aggregation index 6/6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_contacts_random_aggregation_big" repetitions="50000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_unique_contacts_random_aggregation_big" repetitions="50000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>mean [unique-contact-count] of snails</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="snail_contact_avg_contact_duration_random_aggregation_big" repetitions="50000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="74"/>
    <metric>ticks</metric>
    <metric>aggregation-index</metric>
    <metric>avg-contact-duration</metric>
    <enumeratedValueSet variable="snail-type">
      <value value="&quot;Helisoma trivolvis&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-between-meals">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="snail-feasting-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="&quot;random aggregation index&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-mode">
      <value value="&quot;random movement&quot;"/>
      <value value="&quot;resource tracking&quot;"/>
      <value value="&quot;optimal foraging&quot;"/>
      <value value="&quot;variable turn radius&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ZG_sims_4" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15"/>
    <metric>resource-level</metric>
    <metric>report-edge-percent</metric>
    <metric>report-patch-time</metric>
    <metric>Ss</metric>
    <metric>Is</metric>
    <metric>Rs</metric>
    <metric>avg-contacts</metric>
    <metric>total-contacts</metric>
    <metric>report-patches-visited</metric>
    <metric>report-patch-time</metric>
    <enumeratedValueSet variable="starting-infections">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-rate">
      <value value="0.001"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="14"/>
      <value value="16"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectiousness">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recover-time">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GUD">
      <value value="18.75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ZG_sims_SIR" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>resource-level</metric>
    <metric>report-edge-percent</metric>
    <metric>report-patch-time</metric>
    <metric>Ss</metric>
    <metric>Is</metric>
    <metric>Rs</metric>
    <metric>avg-contacts</metric>
    <metric>total-contacts</metric>
    <metric>report-patches-visited</metric>
    <enumeratedValueSet variable="starting-infections">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-rate">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="8"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="40"/>
      <value value="48"/>
      <value value="56"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectiousness">
      <value value="1"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recover-time">
      <value value="1"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GUD">
      <value value="21.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="FinalSim_15May23" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>resource-level</metric>
    <metric>report-edge-percent</metric>
    <metric>report-patch-time</metric>
    <metric>Ss</metric>
    <metric>Is</metric>
    <metric>Rs</metric>
    <metric>avg-contacts</metric>
    <metric>total-contacts</metric>
    <metric>report-patches-visited</metric>
    <metric>avg-unique-contacts</metric>
    <metric>average-contact-duration</metric>
    <enumeratedValueSet variable="starting-infections">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-rate">
      <value value="0.035"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="8"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="40"/>
      <value value="48"/>
      <value value="56"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
      <value value="8"/>
      <value value="18"/>
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectiousness">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recover-time">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GUD">
      <value value="21.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="JeremyRoTestSims" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>resource-level</metric>
    <metric>report-edge-percent</metric>
    <metric>report-patch-time</metric>
    <metric>Ss</metric>
    <metric>Is</metric>
    <metric>Rs</metric>
    <metric>avg-contacts</metric>
    <metric>total-contacts</metric>
    <metric>report-patches-visited</metric>
    <metric>avg-unique-contacts</metric>
    <metric>average-contact-duration</metric>
    <enumeratedValueSet variable="starting-infections">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-rate">
      <value value="0.035"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-algae-coverage">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-snails">
      <value value="8"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="40"/>
      <value value="48"/>
      <value value="56"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity">
      <value value="0"/>
      <value value="8"/>
      <value value="18"/>
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectiousness">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="algae-distribution">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recover-time">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GUD">
      <value value="21.8"/>
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
