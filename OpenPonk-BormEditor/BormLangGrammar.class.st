"
I define and parse grammar of BORM Lang.

!! Synchronization
The inverse operation (taking model and producing DSL) is provided by BormLangExporter

!! Example

Also see BormDiagramExamples for some examples.
[[[
'Person ""Customer"" {
	initial state ""wants pizza""
		go to ""order"" if ""hungry""
	
	activity ""order""
		send ""pizza choice"" to ""Operator::accepts order"" and receive ""confirmation""
		go to ""wait""
	
	state ""wait for pizza"" as ""wait""
		go to ""pay""
	
	activity ""accepts pizza and pays"" as ""pay""
		go to ""final""
	
	final state ""purchase completed"" as ""final""
}
System ""Operator"" {
	activity ""accepts order""
}
' asBorm open
]]]
"
Class {
	#name : 'BormLangGrammar',
	#superclass : 'PPCompositeParser',
	#instVars : [
		'participants',
		'participant',
		'participantType',
		'string',
		'space',
		'stateType',
		'state',
		'activity',
		'transition',
		'transitions',
		'newLine',
		'communication',
		'communications',
		'node',
		'nodes',
		'namePair',
		'name',
		'constraint',
		'diagram',
		'activityStart'
	],
	#category : 'OpenPonk-BormEditor-Dsl',
	#package : 'OpenPonk-BormEditor',
	#tag : 'Dsl'
}

{ #category : 'accessing' }
BormLangGrammar >> activity [
"activity
	^ ($( asParser ,nameOpened ,$)asParser ) ==> [ :arr | {$a.arr second} ]"
	^ (activityStart , space , namePair) ==> [ :arr | arr last ]
]

{ #category : 'accessing' }
BormLangGrammar >> activityEnd [
"activityStart
	^ 'activity' asParser"
	^ ')' asParser
	
]

{ #category : 'accessing' }
BormLangGrammar >> activityStart [
	"activityStart
	^ '(' asParser"
	^ 'activity' asParser
]

{ #category : 'accessing' }
BormLangGrammar >> communication [
"communication
	^ (('talk to ' asParser , name) ==> [ :arr | { arr second . nil . nil } ])
		/ (('send ' asParser , name , ' to ' asParser , name , (' and receive ' asParser , name) optional) ==> [ :arr | { arr fourth . arr second . arr fifth ifNotNil: [ arr fifth second ] } ])"
	^ (('talk to ' asParser , name) ==> [ :arr | { arr second . nil . nil } ])
		/ (('send ' asParser , name , ' to ' asParser , name , (' and receive ' asParser , name) optional) ==> [ :arr | { arr fourth . arr second . arr fifth ifNotNil: [ arr fifth second ] } ])
]

{ #category : 'accessing' }
BormLangGrammar >> communications [
	^ communication trim star
]

{ #category : 'accessing' }
BormLangGrammar >> constraint [
	^ (space , 'if' asParser , space , name) optional ==> [ :arr | arr ifNotNil: [ arr fourth ] ]
]

{ #category : 'accessing' }
BormLangGrammar >> diagram [
	^ participants
]

{ #category : 'accessing' }
BormLangGrammar >> name [
		"^  (namePair/((stringEnclosed/ newWord) ==> [:w | {w.w}]))"
	"	^ ($' asParser , string , $' asParser) ==> [ :arr | arr second ]"
	^ ($" asParser , string , $" asParser) ==> [ :arr | arr second ]
]

{ #category : 'accessing' }
BormLangGrammar >> nameOpened [
	"^  (namePairOpened /(string ==> [:w | {w.w}]))"
	"	^ ($' asParser , string , $' asParser) ==> [ :arr | arr second ]"
	^ ($" asParser , string , $" asParser) ==> [ :arr | arr second ]
]

{ #category : 'accessing' }
BormLangGrammar >> namePair [
	"^ ( ((  stringEnclosed /newWord ), space, $: asParser,space,(  stringEnclosed /newWord ) ))
		map: [ :id :sp1 :c :sp2 :nm | { id.nm }]"
	^ (name , ((space , 'as' asParser , space , name) ==> [ :arr | arr fourth ]) optional)
		==> [ :arr | arr first -> (arr second ifNil: [ arr first ]) ]
]

{ #category : 'accessing' }
BormLangGrammar >> namePairOpened [
	^  ((string/space), space, $: asParser,space,(string/space) )
		map: [ :id :sp1 :c :sp2 :nm | { id.nm }]
		"	^ (name , ((space , 'as' asParser , space , name) ==> [ :arr | arr fourth ]) optional)
		==> [ :arr | arr first -> (arr second ifNil: [ arr first ]) ]"
]

{ #category : 'accessing' }
BormLangGrammar >> newLine [
	^ Character cr asParser plus
]

{ #category : 'accessing' }
BormLangGrammar >> node [
"node
	^ (super activity) / (super state )"
	^ activity / state , communications , transitions
]

{ #category : 'accessing' }
BormLangGrammar >> nodes [
	^ node trim star
]

{ #category : 'accessing' }
BormLangGrammar >> participant [
"
	^ (   space, participantType, space,name,space, ${ asParser ,space, space , $} asParser ,space) map: [ :sp1 :tp :sp2 :pair :sp3 :br :sp   :sp4 :br2 :sp5 |{  tp.pair } 
			  ]
		participant
	^ (participantType , space , namePair , space , ${ asParser , space , nodes
		, space , $} asParser) map: [ :type :sp :pair :sp2 :bra :sp3 :vertices :sp4 :ket
 | { type . pair . vertices }
			  ]"
	^ (participantType , space , namePair , space , ${ asParser , #newline asParser , nodes
		, #newline asParser optional , $} asParser) map: [ :type :sp :pair :sp2 :bra :nl :vertices :nl2 :ket
 | { type . pair . vertices }
			  ]

]

{ #category : 'accessing' }
BormLangGrammar >> participantType [
	^ 'Person' asParser / 'Organization' asParser / 'System' asParser
]

{ #category : 'accessing' }
BormLangGrammar >> participants [
	^ participant trim star
]

{ #category : 'accessing' }
BormLangGrammar >> space [
	"^#space asParser star"
	^ Character space asParser plus
]

{ #category : 'accessing' }
BormLangGrammar >> start [
	^ diagram end
]

{ #category : 'accessing' }
BormLangGrammar >> state [
		"state
	^ (stateType , space , nameOpened , space, stateEnd)
		map: [ :type :sp :n :sp2 :end | 
			{type . n} ]"
	^ (stateType , space) optional , 'state' asParser , space , namePair
		map: [ :type :smb :sp :pair | 
			{(type ifNotNil: [ type first ]).
			pair} ]
]

{ #category : 'accessing' }
BormLangGrammar >> stateEnd [
	"^ ('final' asParser / 'initial and final' asParser / 'initial' asParser) flatten"
	^ (']]' asParser / ']' asParser ) flatten

]

{ #category : 'accessing' }
BormLangGrammar >> stateType [
	"^ ('[[*' asParser / '[*' asParser / '[[' asParser/ '[' asParser) flatten"
	^ ('final' asParser / 'initial and final' asParser / 'initial' asParser) flatten
]

{ #category : 'accessing' }
BormLangGrammar >> string [
	"^ (space,newWord ,space ) plus flatten"
	^ (#word asParser / Character space asParser / '::' asParser) plus flatten
]

{ #category : 'accessing' }
BormLangGrammar >> stringEnclosed [
	"	^ (#word asParser / Character space asParser / '::' asParser) plus flatten"
	^ ($" asParser, (string/space) ,$" asParser ) map: [ :u1 :st :u2 | st ]

]

{ #category : 'accessing' }
BormLangGrammar >> transition [
		"transition
	^ ((activity, space , state)/(state,space,activity ))
		map: [ :f :sp :t| 
			{f first . t first} ]"
	^ 'go to ' asParser , name , constraint
		map: [ :goTo :pair :condition | 
			{pair. condition} ]
]

{ #category : 'accessing' }
BormLangGrammar >> transitions [
	^ transition trim star
]
