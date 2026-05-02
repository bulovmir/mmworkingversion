"
I take a BORM model and produce its BormLang string representation.

!! Synchronization
The inverse operation (taking DSL and parsing it) is provided by BormLangGrammar/BormLangParser
"
Class {
	#name : 'BormLangSerializer',
	#superclass : 'Object',
	#instVars : [
		'diagram',
		'transitionsList',
		'nodesList',
		'activityNodes'
	],
	#category : 'OpenPonk-BormEditor-Dsl',
	#package : 'OpenPonk-BormEditor',
	#tag : 'Dsl'
}

{ #category : 'public-exporting' }
BormLangSerializer class >> export: aDiagram [
	^ self new diagram: aDiagram
]

{ #category : 'converting' }
BormLangSerializer >> activity: anActivity [
|val|
val :=(nodesList at:anActivity).
^val second ifTrue:[val third ifFalse:[val at:3 put: true. nodesList at: anActivity put: val. '(', (self fullNameOpened: anActivity),')'  ] ifTrue:[ self idClosed:anActivity ]] ifFalse: [ '(', (self nameOpened: anActivity),')' ]
	
]

{ #category : 'converting' }
BormLangSerializer >> activityNode: anActivity [
	^ (self activity: anActivity) , (self newLineIf: anActivity sent isNotEmpty), (self communications: anActivity sent)
]

{ #category : 'accessing' }
BormLangSerializer >> addNode: aNode [
|val|
val:= nodesList at: aNode ifAbsentPut: [ ((aNode isKindOf: BormActivity) and: [aNode communications isNotEmpty])
		ifTrue:[{0.true.false}] ifFalse: [{0.false.false} ]].
val at:1 put: (val first +1).
val first >1 ifTrue: [ val at:2 put:true ].
nodesList at: aNode put: val.

]

{ #category : 'converting' }
BormLangSerializer >> collectCommunications [
	^ (activityNodes collect: [ :each | (self communications: each communications) ])asArray joinUsing: self newLine,self newLine
]

{ #category : 'converting' }
BormLangSerializer >> communication: aCommunication [
	| from to con flows  |
	from:=aCommunication sender.
	to := aCommunication receiver.
	flows := aCommunication dataFlows.
	con:=aCommunication constraint.

	^ (self idClosed: from owner), ' ',( self idClosed:from),' ' ,(self communicationConstraint: con),' ',(self idClosed: from owner), ' ',( self idClosed:from),' ',(self dataFlows:flows)
]

{ #category : 'converting' }
BormLangSerializer >> communicationConstraint: aConstraint [
	^ aConstraint ifNil: [ '->' ] ifNotNil: [ '-' , (self nameOpened: aConstraint ),'>' ]
]

{ #category : 'converting' }
BormLangSerializer >> communications: aCollection [
	^ ((aCollection select:[ :each | each sender =each owner]) collect: [ :each | self tab , (self communication: each) ]) joinUsing: self newLine
]

{ #category : 'converting' }
BormLangSerializer >> constraint: aConstraint [
	^ aConstraint ifNil: [ '' ] ifNotNil: [ ' if ' , (self name: aConstraint name) ]
]

{ #category : 'converting' }
BormLangSerializer >> dataFlow: aDataFlow [



	^ (aDataFlow isInput ifTrue:['<-o']ifFalse: [ 'o->' ]),' ',(self nameClosed: aDataFlow )
]

{ #category : 'converting' }
BormLangSerializer >> dataFlows: aCollection [
	
	^ (aCollection collect:[:each|(self dataFlow:each)])joinUsing: ' '
]

{ #category : 'exporting' }
BormLangSerializer >> export: aModel [

	^ self ormodel: aModel
]

{ #category : 'converting' }
BormLangSerializer >> fullNameClosed: aNode [ 
^   (self idClosed:aNode),':',(self nameClosed: aNode)
]

{ #category : 'converting' }
BormLangSerializer >> fullNameOpened: aNode [ 
^ ( self idOpened: aNode) ,':',(self nameOpened:aNode )
]

{ #category : 'converting' }
BormLangSerializer >> idClosed: aNode [ 
^ self nameClosed:aNode 
]

{ #category : 'converting' }
BormLangSerializer >> idOpened: aNode [ 
^ self nameOpened:aNode 
]

{ #category : 'converting' }
BormLangSerializer >> name: aString [
	^ '"' , aString , '"'
]

{ #category : 'converting' }
BormLangSerializer >> nameClosed: aNode [
	^  '"',aNode name,'"'
	
]

{ #category : 'converting' }
BormLangSerializer >> nameOpened: aNode [
	^  aNode name
]

{ #category : 'accessing' }
BormLangSerializer >> newLine [
	^ Character cr asString
]

{ #category : 'accessing' }
BormLangSerializer >> newLineIf: aBoolean [
	^ aBoolean
		ifTrue: [ self newLine ]
		ifFalse: [ '' ]
]

{ #category : 'converting' }
BormLangSerializer >> node: aNode [
	^ (aNode isKindOf: BormActivity)
		ifTrue: [activityNodes add:aNode .self activity: aNode ]
		ifFalse: [ self state: aNode ]
]

{ #category : 'converting' }
BormLangSerializer >> nodes: aCollection [
	^ (aCollection collect: [ :each | self tab,self tab, (self node: each) ]) joinUsing: self newLine , self newLine
]

{ #category : 'converting' }
BormLangSerializer >> ormodel: aModel [
activityNodes := Set new.
	^( self participants: aModel participants),
	(self collectCommunications).
]

{ #category : 'converting' }
BormLangSerializer >> participant: aParticipant [
	^ (self participantType: aParticipant type) , ' ' , (self fullNameClosed: aParticipant ) , ' ' , '{' , self newLine
		, (self transitions: aParticipant nodes) , self newLine , '}', self newLine 
]

{ #category : 'converting' }
BormLangSerializer >> participantType: aType [
	^ aType asString
]

{ #category : 'converting' }
BormLangSerializer >> participants: aCollection [
	^ (aCollection collect: [ :each | self participant: each ])
		joinUsing: self newLine , self newLine
]

{ #category : 'converting' }
BormLangSerializer >> state: aState [
|val|
val :=(nodesList at:aState ).
^val second ifTrue:[val third ifFalse:[val at:3 put: true. nodesList at: aState put: val. (self stateType: aState ), (self fullNameOpened: aState ),(self stateEnd:aState)  ] ifTrue:[ self idClosed:aState ]] ifFalse: [ (self stateType: aState ), (self nameOpened: aState ),(self stateEnd:aState) ]

	
]

{ #category : 'converting' }
BormLangSerializer >> stateEnd: aState [

	aState isFinal
		ifTrue: [ ^ ']]' ].

	^ ']'
]

{ #category : 'converting' }
BormLangSerializer >> stateType: aState [
	(aState isInitial and: [aState isFinal])
		ifTrue: [ ^ '[[*' ].

	aState isInitial
		ifTrue: [ ^ '[*' ].

	aState isFinal
		ifTrue: [ ^ '[[' ].

	^ '['
]

{ #category : 'accessing' }
BormLangSerializer >> tab [
	^ Character tab asString
]

{ #category : 'converting' }
BormLangSerializer >> transition: nodes [
	^ '{' ,( (nodes collect:[:each|(self transitionConstraint: each first),(self node:each second) ]) joinUsing: self newLine) ,'}'
]

{ #category : 'converting' }
BormLangSerializer >> transitionConstraint: aConstraint [
	^ aConstraint ifNil: [ '' ] ifNotNil: [ '-' , (self nameOpened: aConstraint ),'- ' ]
]

{ #category : 'converting' }
BormLangSerializer >> transitions: nodes [
|col ls|
nodesList:= Dictionary new.
transitionsList :=OrderedCollection new.

nodes do: [ :each|  each transitions ifEmpty: [ transitionsList add:(OrderedCollection new add: {nil.each};yourself). self addNode:each. ] ifNotEmpty: [each transitions do:[:transition | transition  source= each ifTrue:[|foundIndex newTransition connectTo |foundIndex:=transitionsList findFirst:[:t|t first second= transition target   ] .foundIndex =0 ifTrue: [ newTransition := ((OrderedCollection new) add:{nil. each}; add: {transition constraint.transition target};yourself ).newTransition inspect. self addNode:transition target.] ifFalse: [ (((transitionsList at:foundIndex)first )at:1 put:transition constraint). newTransition:=((transitionsList at:foundIndex)  addFirst: {nil.each};yourself).transitionsList removeAt: foundIndex .]. connectTo:= transitionsList findFirst:[:tr| tr last second =each  ]. connectTo =0  ifTrue:[ transitionsList add: newTransition.self addNode:each.] ifFalse: [ (transitionsList at:connectTo put:(((transitionsList at:connectTo)),(newTransition removeFirst;yourself )) )]]]]].
ls:=transitionsList sorted:[:a :b | a size > b size].
col:=(ls collect:[:each| self tab , self tab,(self transition:each)])joinUsing: self newLine.
nodesList removeAll .
transitionsList removeAll.
 ^col


	"^ (aCollection collect: [ :each | self tab , self tab , (self transition: each) ]) joinUsing: self newLine"
]
