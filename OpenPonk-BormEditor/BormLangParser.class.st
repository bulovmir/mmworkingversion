"
I take parsed Borm DSL and produce instances of Borm Model.
"
Class {
	#name : 'BormLangParser',
	#superclass : 'BormLangGrammar',
	#instVars : [
		'incompleteTransitions',
		'nodesLookup',
		'participantsLookup',
		'incompleteCommunications'
	],
	#category : 'OpenPonk-BormEditor-Dsl',
	#package : 'OpenPonk-BormEditor',
	#tag : 'Dsl'
}

{ #category : 'accessing' }
BormLangParser class >> ignoredNames [
	^ super ignoredNames, #(incompleteTransitions nodesLookup incompleteCommunications participantsLookup)
]

{ #category : 'accessing' }
BormLangParser >> activity [
	"activity
	^ super activity ==> [ :pair | nodesLookup at: pair value put: (BormActivity named: pair key) ]
	activity
	^ super activity ==> [ :pair | nodesLookup at: pair first put: (BormActivity named: pair second) ]"
	^ super activity ==> [ :pair | nodesLookup at: pair value put: (BormActivity named: pair key) ]

]

{ #category : 'accessing' }
BormLangParser >> communication [
	^ super communication
		==> [ :arr | 
			| comm |
			comm := BormCommunication new.
			comm receiver: arr first.
			arr second ifNotNil: [ comm addDataFlowNamed: arr second ].
			arr third ifNotNil: [ (comm addDataFlowNamed: arr third) beOutput ].
			comm ]
]

{ #category : 'accessing' }
BormLangParser >> constraint [
	^ super constraint ==> [ :condition | condition ifNotNil: [ BormConstraint named: condition ] ]
]

{ #category : 'accessing' }
BormLangParser >> diagram [
"diagram
	^ super diagram
		==> [ :arr | 
			| d |
			d := BormORModel new.
			d addAll: arr.

			d ]"
	^ super diagram
		==> [ :arr | 
			| d |
			d := BormORModel new.
			d addAll: arr.

			incompleteCommunications
				do: [ :each | 
					| n splitName |
					splitName := (each receiver splitOn: '::').
					n := (participantsLookup at: splitName first) at: splitName second.
					n receive: each.
					each receiver: n ].

			d ]
]

{ #category : 'initialization' }
BormLangParser >> initialize [
	super initialize.
	self resetCache
]

{ #category : 'accessing' }
BormLangParser >> node [
	^ super node
		map: [ :n :comms :trans | 
			trans
				do: [ :each | 
					each source: n.
					n addTransition: each ].
			incompleteTransitions addAll: trans.

			comms
				do: [ :each | 
					each sender: n.
					n send: each ].
			incompleteCommunications addAll: comms.
			n ]
]

{ #category : 'parsing' }
BormLangParser >> parseStatements: aString [
	^ self parse: aString
]

{ #category : 'accessing' }
BormLangParser >> participant [
"	^ super participant
		==> [ :arr | 
			| p |
			p := BormParticipant new
				name: arr second second;
				type: arr first;
				yourself.
				nodesLookup := Dictionary new.
				arr third do: [ :each|{ each first = $a ifTrue: [ p add: (self activity: each second )] ifFalse: [ p add:(self state:each   )] } ].
				participantsLookup at: arr second first put: nodesLookup.
				p ]
				"
	^ super participant
		==> [ :arr | 
			| p |
			p := BormParticipant new
				name: arr second key;
				type: arr first;
				yourself.
			"insert all nodes"
			arr third do: [ :each | p add: each ].
			"finish transitions"
			incompleteTransitions
				do: [ :each | 
					| n |
					n := nodesLookup at: each target.
					n addTransition: each.
					each target: n ].
			incompleteTransitions removeAll.
			participantsLookup at: arr second value put: nodesLookup.
			nodesLookup := Dictionary new.
			p ]
]

{ #category : 'accessing' }
BormLangParser >> participantType [
	^ super participantType ==> [ :str | BormParticipantType perform: str asSymbol ]
]

{ #category : 'parsing' }
BormLangParser >> replaceModel: aModel withStatements: parsedModel [
	aModel elements reset.
	parsedModel elements do: [ :each | aModel add: each ]
]

{ #category : 'private' }
BormLangParser >> resetCache [
	nodesLookup := Dictionary new.
	participantsLookup := Dictionary new.
	incompleteTransitions := OrderedCollection new.
	incompleteCommunications := OrderedCollection new
]

{ #category : 'accessing' }
BormLangParser >> state [
"state
	^ super state
		map: [ :arr | 
			| s t |
			t := arr first.
			s := BormState named: arr second second .
			nodesLookup at: arr second first put: s.
			t
				ifNotNil: [ 
					t = '[*'
						ifTrue: [ s beInitial ].
					t = '[['
						ifTrue: [ s beFinal ].
					t = '[[*'
						ifTrue: [ 
							s
								beInitial;
								beFinal ] ].
			s ]"
	^ super state
		map: [ :arr | 
			| s t |
			t := arr first.
			s := BormState named: arr second key.
			nodesLookup at: arr second value put: s.
			t
				ifNotNil: [ 
					t = 'initial'
						ifTrue: [ s beInitial ].
					t = 'final'
						ifTrue: [ s beFinal ].
					t = 'initial and final'
						ifTrue: [ 
							s
								beInitial;
								beFinal ] ].
			s ]
]

{ #category : 'accessing' }
BormLangParser >> transition [
		"transition
	^ super transition
		==> [ :arr | 
			| t |
			t := BormTransition new.
			t source: arr first.
			t target: arr second.
			t ]"
	^ super transition
		==> [ :arr | 
			| t |
			t := BormTransition new.
			arr second ifNotNil: [ t constraint: arr second ].
			t target: arr first.
			t ]
]
