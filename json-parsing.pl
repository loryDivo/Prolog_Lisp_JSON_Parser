%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Membri del gruppo (cognome, nome	matricola):
%	Belotti, Manuel	795599
%	Di Vito, Lorenzo	793128
%	Lobba, Diego		795702
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				JSON Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Introduzione:
%	Json è un tipo di formato di scambio dati in ambito server.
%	E' una valida alternativa al formato XML e molti browser
%	mettono a disposizione entrambe le possibilità di integrazione.
%	L'obiettivo di questo progetto è quello di creare un Parser
%	che sia in grado di interpretare una stringa di input JSON
%	e manipolarla secondo le diverse strutture dati presenti
%	nei due linguaggi di programmazione: Prolog e Common Lisp.
%
%	Sia nel linguaggio di programmazione Prolog che in Common Lisp
%	per poter programmare il Parser è stata adottata la tecnica
%	"Parsing-by-consumption".
%	Consiste in un Parser che analizza l'input "matchando" un
%	simbolo alla volta e affidando, non appena non metcha il primo simbolo,
%	il resto al predicato/funzione successivo.
%
%	Il Parser creato è molto compatto, questo lo rende uno strumento
%	affidabile in grado di riconoscere qualsiasi stringa in input
%	conforme al linguaggio JSON, producendo un output formattato
%	secondo le specifiche della consegna.
%
%	La stringa in input nei due linguaggi di programmazione è:
%	-Scomposta in "char" singoli all'interno di una lista, la
%	principale struttura dati sia nel linguaggio di programmazione
%	Prolog che in Common Lisp.
%	Il predicato\funzione principale è JSONPARSE,
%	questa riceve in input la stringa JSON e, se
%	questa rispetta la grammatica della traccia, la formatta,
%	altrimenti il programma fallisce.
%
%	Successivamente, l'output formattato potrà essere scritto su file,
%	se esistente, utilizzando il predicato\funzione JSONWRITE. Nel caso in
%	cui il nome del file non esista verrà creato un nuovo file con il nome
%	dato in input.
%
%	Utilizzando il predicato\funzione JSONLOAD
%	è possibile, inserendo il nome di un file, poter
%	leggerne il contenuto riportando in output la corretta stringa JSON
%	e, in caso di file non esistente, il programma fallirà.
%
%	Si noti che sia in Prolog che in Common Lisp sono state usate
%	predicati/funzioni che lavorano sul percorso assoluto, non saranno
%	quindi validi i simobli di "home" presenti in UNIX.
%
%	Infine un ultimo predicato\funzione principale è JSONGET,
%	che, prendendo in input un indice,
%	restituisce, nel caso di un Array, l'atomo nella posizione riferita
%	all' indice inserito.
%	Nel caso di un Object, dato un atomo,
%	restituirà il successivo dell'ultimo
%	atomo trovato con le specifiche dell'input inserito.
%	In caso di indice\atomo non Pertinenteo all' output,
%	il programma fallirà.
%
%	La grammatica JSON fornita è la seguente:
%
%	JSON ::= Object | Array
%	Object ::= '{}' | '{' Members '}'
%	Members ::= Pair | Pair ',' Members
%	Pair ::= ( String | Identifier ) ':' Value
%	Array ::= '[]' | '[' Elements ']'
%	Elements ::= Value | Value ',' Elements
%	Value ::= JSON | Number | String
%	Number ::= Digit+ | Digit+ '.' Digit+
%	Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
%	String ::= '"' AnyChar* '"' | '’' AnyChar* '’'
%	Identifier ::= Char | Char (Digit | Char)*
%	AnyChar ::= (carattere diverso da '"' e da '’') | '\"' | '\’'
%	Char ::= (un carattere alfabetico: a-z o A-Z)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




/*********FATTI*E*REGOLE*DI*USO*GENERALE*****************************/

%% Questi predicati si assicurano che il loro argomento
%% sia un singolo oppure un doppio apice.

is_single_quote('\'').
is_double_quote('\"').

is_quote(X) :- is_single_quote(X).
is_quote(X) :- is_double_quote(X).


%% Questi predicati si assicurano che il loro argomento
%% sia una parentesi (aperta o chiusa, quadra o graffa).

is_osb('[').
is_csb(']').
is_ocb('{').
is_ccb('}').


%% Questi predicati si assicurano che il loro argomento sia un
%% simbolo di punteggiatura (virgola, due punti, punto o backslash).

is_comma(',').
is_colon(':').
is_dot('.').
is_backslash('\\').


%% is_whitespace/1 si assicura che il suo argomento sia
%% uno spazio bianco, un "tab" oppure un "newline".

is_whitespace(' ').
is_whitespace('\t').
is_whitespace('\n').

/*********FUNZIONI*GENERICHE*****************************************/

%% skip_whitespace/2 si occupa della gestione di spazi bianchi,
%% tab e newline presenti negli oggetti JSON inseriti come
%% primo argomento di jsonparse/2. Le prime due definizioni sono
%% caso base e passo ricorsivo volti ad ignorare i suddetti caratteri
%% presenti nella lista Codes, mentre l'ultimo caso restituisce
%% il controllo alla procedura chiamante non appena viene trovato
%% un carattere diverso dai tre che verificano le due versioni
%% precedenti, restituendole inoltre la lista dei caratteri
%% non ancora esaminati. E' un predicato fondamentale per il corretto
%% funzionamento di questo progetto, e sarà utilizzato molto spesso
%% nei predicati di supporto a jsonparse/2.

skip_whitespace([], []) :- !.

skip_whitespace([H | Codes], Remains) :-
    is_whitespace(H),
    skip_whitespace(Codes, Remains), !.

skip_whitespace([H | Remains], [H | Remains]) :-
    \+(is_whitespace(H)), !.


%% is_list_empty/1 verifica che il suo argomento sia una lista vuota.

is_list_empty([]).


%% is_empty/2 verifica che i suoi argomenti siano due liste
%% con la stessa lunghezza.
%% La definizione di questo predicato è necessaria, poichè ci
%% permette di verificare la presenza di elementi JSON durante
%% il parsing.

is_empty(Codes, Remains) :-
    length(Codes, LenCodes),
    length(Remains, LenRemains),
    LenCodes is LenRemains.

/******************************JSONPARSE*******************************/

%% jsonparse/2 si occupa semplicemente di verificare che
%% JSONString sia un atomo e Object una variabile,
%% dopodichè converte JSONString in una lista di caratteri (Chars)
%% e cede il controllo a is_json/3; una volta ripreso il controllo
%% verifica che la lista Remains sia vuota.

jsonparse(JSONString, Object) :-
    atom(JSONString),
    var(Object),
    atom_chars(JSONString, Chars),
    is_json(Chars, Remains, Object),
    is_list_empty(Remains), !.

%% Questa versione di jsonparse/2 verifica che Object sia identico
%% a ObjAtom, che è il risultato della (eventuale) conversione di
%% JSONString in un oggetto JSON effettuata tramite la chiamata di is_json/3.

jsonparse(JSONString, Object) :-
    atom(JSONString),
    nonvar(Object),
    atom_chars(JSONString, Chars),
    is_json(Chars, Remains, ObjAtom),
    is_list_empty(Remains), !,
    Object = ObjAtom.

%% Questa è la versione "reverse" di jsonparse/2, che converte
%% l'oggetto JSON Object nella sua rappresentazione atomica;
%% per fare ciò, verifica che JSONString sia una variabile e che
%% Object non lo sia, dopodichè cede il controllo a is_json/2.

jsonparse(JSONString, Object) :-
    var(JSONString),
    nonvar(Object),
    is_json(JSONString, Object),
    !.

/*********IS*JSON****************************************************/

%% is_json/3 non fa altro che chiamare skip_whitespace/2 prima e dopo
%% aver ceduto il controllo a is_array/2 o a is_object/2.

is_json(Codes, Remains, Array) :-
    skip_whitespace(Codes, CodesWs),
    is_array(CodesWs, RemainsWs, Array),
    skip_whitespace(RemainsWs, Remains).

is_json(Codes, Remains, Object) :-
    skip_whitespace(Codes, CodesWs),
    is_object(CodesWs, RemainsWs, Object),
    skip_whitespace(RemainsWs, Remains).

%% is_json/2, in base a ciò che è
%% JSON_obj (jsonobj(_) oppure jsonarray(_)),
%% cede il controllo a is_object/2 oppure a is_array/2.

is_json(StringJSON, JSON_obj ) :-
    JSON_obj = jsonobj(_),
    is_object(StringJSON, JSON_obj).

is_json(StringJSON, JSON_obj ) :-
    JSON_obj = jsonarray(_),
    is_array(StringJSON, JSON_obj).

/*********IS*ARRAY***************************************************/

%% is_array/3 ha due definizioni: una per gestire l'Array vuoto
%% e una per gestire l'Array con elements; nel primo caso
%% viene semplicemente verificato che il primo argomento
%% contenga solamente due parentesi quadre, una aperta e una
%% chiusa, più eventuali caratteri la cui gestione è affidata a
%% skip_whitespace/2, mentre nel secondo caso viene verificata la
%% presenza della parentesi quadra aperta, viene ceduto il controllo
%% a is_element/3 e una volta ripreso il controllo viene verificata
%% la presenza della parentesi quadra chiusa, il tutto unito con due
%% chiamate a skip_whitespace/2.

is_array([H | Codes], Remains, jsonarray([]) ) :-
    is_osb(H),
    skip_whitespace(Codes, [Close | RemainsWs]),
    is_csb(Close),
    skip_whitespace(RemainsWs, Remains).

is_array([H | Codes], Remains, jsonarray(Elements) ) :-
    is_osb(H),
    skip_whitespace(Codes, CodesWs),
    is_element(CodesWs, [Close | RemainsWs], Elements),
    is_csb(Close),
    skip_whitespace(RemainsWs, Remains).

%% Anche is_array/2 ha due definizioni: nella prima viene
%% gestito l'Array vuoto mentre nell'altra l'Array con uno
%% o più Elements che saranno gestiti da is_element/2, il cui
%% risultato verrà poi combinato da is_array/2 tramite il
%% predicato atomic_list_concat/3; in entrambi i casi viene
%% prima accertato che StringArray sia una variabile, e nel
%% secondo caso anche che Element non lo sia.

is_array(StringArray, jsonarray([])) :-
    var(StringArray),
    StringArray = '[]'.

is_array(StringArray, jsonarray(Element)) :-
	var(StringArray),
	nonvar(Element),
	is_element(StringElement, Element),
	atomic_list_concat(['[', StringElement, ']'], ' ', StringArray).

/*********IS*ELEMENT*************************************************/

%% is_element/3 ha due definizioni: una per il caso base, cioè
%% il caso in cui c'è solo un Element da esaminare, e una per il
%% passo ricorsivo, cioè il caso in cui ci sono più Elements da
%% esaminare. Nel primo caso viene affidato il controllo a is_value/3,
%% dopodichè viene fatto un controllo sulla lunghezza delle liste
%% contenenti i caratteri esaminati e da esaminare e infine viene
%% verificato che l'ultimo carattere sia una parentesi quadra chiusa
%% per accertarsi effettivamente che l'Element sia l'ultimo; nel
%% secondo caso, dopo aver ceduto il controllo a is_value/3,
%% viene verificato che il primo carattere di quelli ancora da esaminare
%% sia una virgola, dopodichè viene chiamato ricorsivamente is_element/3
%% per esaminare gli altri Elements.

is_element(Codes, [Close | Remains], [Value]) :-
    skip_whitespace(Codes, CodesWs),
    is_value(CodesWs, [Close | RemainsWs], Value),
    \+(is_empty(CodesWs, [Close | RemainsWs])),
    skip_whitespace(RemainsWs, Remains),
    is_csb(Close).

is_element(Codes, Remains, [Value | MoreElements] ) :-
    skip_whitespace(Codes, CodesWs),
    is_value(CodesWs, [Comma | ElementWs], Value),
    skip_whitespace(ElementWs, Element),
    is_comma(Comma),
    is_element(Element, RemainsWs, MoreElements),
    skip_whitespace(RemainsWs, Remains).

%% Come is_element/3, anche la versione "reverse" ha
%% un caso base (un Element solo) e un caso ricorsivo
%% (più Elements); in entrambi i casi si cede il controllo
%% a is_value/2 (dopo aver controllato che StringElement
%% sia una variabile) e nell'ultimo si richiama poi ricorsivamente
%% is_element/2 per esaminare i restanti Elements, per poi combinare
%% i risultati ottenuti con atomic_list_concat/3.

is_element(StringElement, [Value]) :-
    var(StringElement),
    nonvar([Value]),
    is_value(StringValue, Value),
    StringElement = StringValue.

is_element(StringElement, [Value | MoreElements]) :-
    var(StringElement),
    nonvar([Value | MoreElements]),
    is_value(StringValue, Value),
    is_element(StringMrElements, MoreElements),
    atomic_list_concat([StringValue, StringMrElements], ' , ', StringElement).

/*********IS*VALUE***************************************************/

%% is_value/3 non fa altro che rivolgersi a skip_whitespace/2
%% prima e dopo aver ceduto il controllo a is_string/3,
%% is_number/3 o is_json/3.

is_value(Codes, Remains, String) :-
    skip_whitespace(Codes, CodesWs),
    is_string(CodesWs, RemainsWs, String),
    skip_whitespace(RemainsWs, Remains).

is_value(Codes, Remains, Number) :-
    skip_whitespace(Codes, CodesWs),
    is_number(CodesWs, RemainsWs, Number),
    skip_whitespace(RemainsWs, Remains).

is_value(Codes, Remains, Value) :-
    skip_whitespace(Codes, CodesWs),
    is_json(CodesWs, RemainsWs, Value),
    skip_whitespace(RemainsWs, Remains).

%% is_value/2 si comporta come is_value/3: verifica che StringValue
%% sia una variabile, che il suo secondo argomento non lo sia e poi
%% cede il controllo a is_string/2, is_number/2 o a is_json/2.

is_value(StringValue, Number) :-
    var(StringValue),
    nonvar(Number),
    is_number(StringValue, Number),
    !.

is_value(StringValue, String) :-
    var(StringValue),
    nonvar(String),
    is_string(StringValue, String),
    !.

is_value(StringValue, JSON_obj) :-
    var(StringValue),
    nonvar(JSON_obj),
    is_json(StringValue, JSON_obj).

/*********IS*OBJECT**************************************************/

%% Il comportamento di is_object/3 è analogo a quello di is_array/3,
%% con la differenza che viene verificata la presenza delle parentesi
%% graffe (non quadre) e che il controllo nel secondo caso viene ceduto
%% a is_member/3 (non a is_element/3).

is_object([H | Codes], Remains, jsonobj([])) :-
    is_ocb(H),
    skip_whitespace(Codes, [Close | RemainsWs]),
    is_ccb(Close),
    skip_whitespace(RemainsWs, Remains).

is_object([H | Codes], Remains, jsonobj(Member)) :-
    is_ocb(H),
    skip_whitespace(Codes, CodesWs),
    is_member(CodesWs, [Close | RemainsWs], Member),
    skip_whitespace(RemainsWs, Remains),
    is_ccb(Close).

%% Anche qui il comportamento è analogo a quello di is_array/2, con
%% la gestione dei Members dell'Object affidata a is_member/2.

is_object(StringObject, jsonobj([])) :-
    var(StringObject),
    nonvar(jsonobj([])),
    StringObject = '{}'.

is_object(StringObject, jsonobj(Member)) :-
    var(StringObject),
    nonvar(Member),
    is_member(StringMember, Member),
    atomic_list_concat(['{', StringMember, '}'], ' ', StringObject).

/*********FOUND*NEXT*************************************************/

%% found_next/2 è fondamentale per il corretto funzionamento
%% di is_member/2. Tale predicato verifica se data una pair
%% ve ne sono altre aventi il suo stesso attributo.

found_next([(Att, _)], (AttExamined, _)) :-
    =(Att, AttExamined),
    !.

found_next([(Att, _) | _], (AttExamined, _)) :-
    =(Att, AttExamined),
    !.

found_next([(_, _) | OtherPairs], (AttExamined, _)) :-
    found_next(OtherPairs, (AttExamined, _)).

/*********IS*MEMBER**************************************************/

%% is_member/3 ha tre definizioni; la prima riguarda la gestione
%% del caso base ed è analoga a quella di is_element/3 (una sola Pair,
%% gestione affidata a is_pair/3, controllo sulla lunghezza delle liste
%% e sulla presenza della parentesi graffa chiusa) mentre le altre
%% riguardano la gestione del caso ricorsivo, anch'essa analoga a quella di
%% is_element/3 (più Pairs insieme, controllo della prima Pair affidata
%% a is_pair/3, verifica della presenza della virgola, chiamata ricorsiva
%% a is_member/3 per la gestione della altre Pairs). E' stato necessario
%% progettare due definizioni per il caso ricorsivo per permettere la
%% rimozione dei duplicati delle Pairs tramite found_next/2: nella prima
%% definizione la prima Pair esaminata viene conservata, mentre
%% nell'ultima viene scartata.

is_member(Codes, [Close | Remains], [Pair] ) :-
    skip_whitespace(Codes, CodesWs),
    is_pair(CodesWs, [Close | RemainsWs], Pair),
    \+(is_empty(CodesWs, [Close | RemainsWs])),
    is_ccb(Close),
    skip_whitespace(RemainsWs, Remains).

is_member(Codes, Remains, [Pair | MoreMembers]) :-
    skip_whitespace(Codes, CodesWs),
    is_pair(CodesWs, BeforeComma, Pair),
    skip_whitespace(BeforeComma, [Comma | AfterComma]),
    is_comma(Comma),
    skip_whitespace(AfterComma, Member),
    is_member(Member, RemainsWs, MoreMembers),
    \+(found_next(MoreMembers, Pair)),
    skip_whitespace(RemainsWs, Remains).

is_member(Codes, Remains, MoreMembers) :-
    skip_whitespace(Codes, CodesWs),
    is_pair(CodesWs, BeforeComma, Pair),
    skip_whitespace(BeforeComma, [Comma | AfterComma]),
    is_comma(Comma),
    skip_whitespace(AfterComma, Member),
    is_member(Member, RemainsWs, MoreMembers),
    found_next(MoreMembers, Pair),
    skip_whitespace(RemainsWs, Remains).

%% is_member/2 si comporta in modo analogo a is_element/2,
%% sia nel caso base (una sola Pair, verifica sugli argomenti,
%% controllo a is_pair/2) sia nel caso ricorsivo (più Pairs
%% insieme, verifica sugli argomenti, controllo a is_pair/2 per
%% analizzare la prima Pair e poi ricorsivamente a is_member/2
%% per analizzare le altre).

is_member(StringMember, [Pair]) :-
    var(StringMember),
    nonvar([Pair]),
    is_pair(StringPair, Pair),
    StringMember = StringPair.

is_member(StringMember, [Pair | MoreMembers]) :-
    var(StringMember),
    nonvar([Pair | MoreMembers]),
    is_pair(StringPair, Pair),
    is_member(StringMoreMembers, MoreMembers),
    atomic_list_concat([StringPair, StringMoreMembers], ' , ', StringMember).

/*********IS*PAIR****************************************************/

%% is_pair/3 ha quattro definizioni, due per il caso in cui il primo
%% elemento è una String e due nel caso in cui è un Identifier;
%% ciascuna coppia ha una definizione per il caso in cui c'è una sola
%% Pair da analizzare e una nel caso ce ne siano di più, e si distinguono
%% dalla presenza dei predicati is_ccb/1 (Pair singola) e is_comma/1
%% (Pairs multiple). Il modo di operare è comune a tutte e quattro le
%% definizioni: si verifica prima la presenza e la natura del primo elemento
%% della Pair tramite is_string/3 o is_identifier/3 (se è una String,
%% la si converte in atomo con atom_string/2), poi la presenza dei due punti
%% tramite is_colon/1, poi si cede il controllo a is_value/3 e infine
%% si analizza l'ultimo carattere tramite is_ccb/1 o is_comma/1.

is_pair(Codes, [Close | Remains], (StringAtom , IsValue)):-
    skip_whitespace(Codes, CodesWs),
    is_string(CodesWs, BeforeColon, String),
    atom_string(StringAtom, String),
    skip_whitespace(BeforeColon, [Colon | AfterColon]),
    is_colon(Colon),
    skip_whitespace(AfterColon, Value),
    is_value(Value, AfterValue, IsValue),
    skip_whitespace(AfterValue, [Close | RemainsWs]),
    is_ccb(Close),
    skip_whitespace(RemainsWs, Remains).

is_pair(Codes, [Comma | Remains], (StringAtom, IsValue)) :-
    skip_whitespace(Codes, CodesWs),
    is_string(CodesWs, BeforeColon, String),
    atom_string(StringAtom, String),
    skip_whitespace(BeforeColon, [Colon | AfterColon]),
    is_colon(Colon),
    skip_whitespace(AfterColon, Value),
    is_value(Value, AfterValue, IsValue),
    skip_whitespace(AfterValue, [Comma | RemainsWs]),
    is_comma(Comma),
    skip_whitespace(RemainsWs, Remains).

is_pair(Codes, [Close | Remains], (Identifier, IsValue)) :-
    skip_whitespace(Codes, CodesWs),
    is_identifier(CodesWs, BeforeColon, Identifier),
    skip_whitespace(BeforeColon, [Colon | AfterColon]),
    is_colon(Colon),
    skip_whitespace(AfterColon, Value),
    is_value(Value, AfterValue, IsValue),
    skip_whitespace(AfterValue, [Close | RemainsWs]),
    is_ccb(Close),
    skip_whitespace(RemainsWs, Remains).

is_pair(Codes, [Comma | Remains], (Identifier, IsValue)) :-
    skip_whitespace(Codes, CodesWs),
    is_identifier(CodesWs, BeforeColon, Identifier),
    skip_whitespace(BeforeColon, [Colon | AfterColon]),
    is_colon(Colon),
    skip_whitespace(AfterColon, Value),
    is_value(Value, AfterValue, IsValue),
    skip_whitespace(AfterValue, [Comma | RemainsWs]),
    is_comma(Comma),
    skip_whitespace(RemainsWs, Remains).

%% is_pair/2 ha soltanto due definizioni: la prima gestisce il caso
%% in cui il primo elemento della Pair è un Identifier, la seconda
%% invece il caso in cui è una String; in entrambi i casi viene per
%% prima cosa ceduto il controllo al predicato che analizza il primo
%% elemento (is_identifier/2 o is_string/2), poi a is_value/2 che
%% si occuperà del secondo elemento della Pair e infine il risultato
%% di queste chiamate viene messo insieme con atomic_list_concat/3 (nel
%% caso che il primo elemento sia una String, prima di is_value/2 viene
%% fatto un controllo per verificare che NON sia una stringa dal punto di
%% vista di Prolog, dopodichè lo si converte in stringa con atom_string/2).

is_pair(StringPair, (Attribute, Value) ) :-
    var(StringPair),
    nonvar( (Attribute, Value) ),
    is_identifier(StringID, Attribute),
    is_value(StringValue, Value),
    atomic_list_concat([StringID, ':', StringValue], ' ', StringPair).

is_pair(StringPair, (Attribute, Value) ) :-
    var(StringPair),
    nonvar( (Attribute, Value) ),
    \+(string(Attribute)),
    atom_string(Attribute, AttributeString),
    is_string(StringID, AttributeString),
    is_value(StringValue, Value),
    atomic_list_concat([StringID, ':', StringValue], ' ', StringPair).

/*********IS*IDENTIFIER**********************************************/

%% is_identifier/3 ha due definizioni, una nel caso in cui l'Identifier
%% da analizzare è costituito da un solo Char e una nel caso in cui
%% è costituito da un Char seguito da più Digits o Chars; in entrambi
%% i casi viene analizzato il primo Char tramite is_char/1 e viene
%% verificata la presenza dei due punti per poi conventire tutti gli
%% elementi analizzati in un unico atomo, tuttavia nel secondo caso
%% viene aggiunta una chiamata a is_char/3 per analizzare gli altri
%% Chars / Digits dell'Identifier.

is_identifier(Codes, [Colon | Remains], Identifier) :-
    skip_whitespace(Codes, [Char | AfterId]),
    is_char(Char),
    skip_whitespace(AfterId, [Colon | RemainsWs]),
    is_colon(Colon),
    skip_whitespace(RemainsWs, Remains),
    atom_chars(Identifier, [Char]).

is_identifier(Codes, [Colon | Remains], Identifier) :-
    skip_whitespace(Codes, [Char | CharOrDigit]),
    is_char(Char),
    is_char(CharOrDigit, AfterId, IsChar),
    skip_whitespace(AfterId, [Colon | RemainsWs]),
    is_colon(Colon),
    skip_whitespace(RemainsWs, Remains),
    atom_chars(Identifier, [Char | IsChar]).

%% is_identifier/2 si assicura che JSON_ID sia un atomo, poi lo
%% converte in una lista di caratteri il cui primo elemento viene
%% ceduto a is_char/1 e i restanti a is_char/3.

is_identifier(StringID, JSON_ID) :-
    var(StringID),
    nonvar(JSON_ID),
    atom(JSON_ID),
    atom_chars(JSON_ID, [Char | IsChar]),
    is_char(Char),
    is_char(IsChar, [], _),
    atomic_list_concat([ '\"', JSON_ID, '\"'], '', StringID),
    !.

/*********IS*CHAR*(SUPPORTO*A*IDENTIFIER)****************************/

%% is_char/1 verifica che il suo argomento faccia parte della lista
%% contenente esclusivamente le lettere dell'alfabeto inglese, sia
%% maiuscole che minuscole.

is_char(H) :- member(H, [
			 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
			 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
			 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',

			 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
			 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
			 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
			]).

%% Il funzionamento di is_char/3 è identico a quello di
%% skip_whitespace/2: esamina uno per uno gli elementi
%% del primo argomento, che è una lista di caratteri, finchè
%% non trova un carattere che non unifica nè con is_char/1
%% nè con is_digit/1 e restituisce al predicato chiamante
%% una lista contenente gli elementi non ancora esaminati
%% (o una lista vuota se li ha esaminati tutti, caso base).

is_char([], [], []).

is_char([H | Remains], [H | Remains], []) :-
    \+( is_char(H) ),
    \+( is_digit(H) ),
    !.

is_char([H | Codes], Remains, [H | Already]) :-
    is_char(H), !,
    is_char(Codes, Remains, Already).

is_char([H | Codes], Remains, [H | Already]) :-
    is_digit(H), !,
    is_char(Codes, Remains, Already).

/*********IS*NUMBER**************************************************/

%% is_number/3 ha quattro definizioni: una per i numeri in virgola mobile
%% con segno (1), una per i numeri in virgola mobile senza segno (2), una
%% per i numeri interi con segno (3) e una per i numeri interi senza
%% segno (4). Tutte queste definizioni operano allo stesso modo: verificano
%% (1 e 3) o meno (2 e 4) la presenza del segno, cedono il controllo a
%% is_float/3 (1 e 2) o a is_integer/3 (3 e 4) e trasformano la lista di
%% caratteri che ricevono in un atomo tramite atom_chars/2 che viene poi
%% trasformato a sua volta in un numero Prolog tramite atom_number/2.

%[Sign | Codes] o solo Codes contiene atomi (quotati) indicanti numeri
is_number([Sign | Codes], Remains, Number) :-
    is_sign(Sign),
    is_float(Codes, Remains, NumberCodes),
    atom_chars(AtomNumber, [Sign | NumberCodes]),
    atom_number(AtomNumber, Number), !.

is_number(Codes, Remains, Number) :-
    is_float(Codes, Remains, NumberCodes),
    atom_chars(AtomNumber, NumberCodes),
    atom_number(AtomNumber, Number), !.

is_number([Sign | Codes], Remains, Number) :-
    is_sign(Sign),
    is_integer(Codes, Remains, NumberCodes),
    atom_chars(AtomNumber, [Sign | NumberCodes]),
    atom_number(AtomNumber, Number), !.

is_number(Codes, Remains, Number) :-
    is_integer(Codes, Remains, NumberCodes),
    atom_chars(AtomNumber, NumberCodes),
    atom_number(AtomNumber, Number), !.

%% is_number/2, dopo aver effettuato i soliti controlli dei
%% "predicati reverse" sulla "natura" degli argomenti, verifica
%% semplicemente che Number sia un numero Prolog tramite number/1,
%% dopodichè lo trasforma in un atomo tramite atom_number/2.

is_number(StringNumber, Number) :-
    var(StringNumber),
    nonvar(Number),
    number(Number),
    atom_number(StringNumber, Number).

/*********IS*INTEGER*(SUPPORTO*A*IS*NUMBER)**************************/

%% Il funzionamento di is_integer/3 è identico a quello di is_char/3,
%% cioè verifica che tutti gli elementi del primo argomento unifichino
%% con is_digit/1 e appena ne trova uno che non unifica restituisce il
%% controllo al predicato chiamante insieme alla lista di elementi non
%% ancora esaminati (o una lista vuota se li ha esaminati tutti, caso base).

is_integer([], [], []).

is_integer([D | T], Remains, [D | Already]) :-
    is_digit(D),
    is_integer(T, Remains, Already).

is_integer([D | T], [D | T], []) :-
    \+(is_digit(D)).

/*********IS*FLOAT*(SUPPORTO*A*IS*NUMBER)****************************/

%% is_float/3 verifica la presenza di due numeri interi (IntPart e DecPart)
%% prima e dopo il punto (is_dot/1) che ogni numero in virgola mobile
%% deve avere e poi li unifica in un'unica lista (Number).

is_float(Codes, Remains, Number) :-
    is_integer(Codes, [Dot | Decimal], IntPart),
    \+(is_empty(Codes,[Dot | Decimal])), !,
    is_dot(Dot),
    is_integer(Decimal, Remains, DecPart),
    \+(is_empty(Decimal, Remains)), !,
    append(IntPart, ['.' | DecPart], Number).

/*********FATTI*E*REGOLE*DI*SUPPORTO*A*IS*NUMBER*********************/

%% is_sign/1 verifica che il proprio argomento sia
%% un '+' o un '-'.

is_sign('-').
is_sign('+').


%% is_digit/1 ha due definizioni: nella prima viene gestito il caso in cui
%% l'argomento è un numero Prolog (quindi non soddisfa il predicato atom/1),
%% nell'altra il caso in cui l'argomento è un atomo. In entrambi i casi
%% viene verificato che la versione atomica dell'argomento sia un "digit"
%% secondo le regole di Prolog tramite il predicato char_type/2.

is_digit(Number) :-
    \+(atom(Number)),!,
    atom_number(Atom, Number),
    char_type(Atom, digit).

is_digit(Atom) :-
    atom(Atom), !,
    char_type(Atom, digit).

/*********IS*STRING**************************************************/

%% is_string/3 verifica che il primo e l'ultimo carattere
%% della lista Codes siano o singoli (is_single_quote/1)
%% o doppi apici (is_double_quote/1), cedendo nel mezzo il
%% controllo a is_anychar/3 e convertendo il suo risultato
%% (una lista di caratteri) in una stringa Prolog.

is_string([H | Codes], Remains, String) :-
    is_double_quote(H),
    is_anychar(Codes, [Quote | Remains], StringCodes),
    string_codes(String, StringCodes),
    is_double_quote(Quote), !.

is_string([H | Codes], Remains, String) :-
    is_single_quote(H),
    is_anychar(Codes, [Quote | Remains], StringCodes),
    string_codes(String, StringCodes),
    is_single_quote(Quote), !.

%% is_string/2 effettua i soliti controlli iniziali dei
%% "predicati reverse" sugli argomenti, poi verifica che JSON_string
%% sia una stringa Prolog (string/1), la converte prima in atomo
%% (atom_string/2) poi in una lista di caratteri (atom_chars/2) che
%% verrà controllata da is_anychar/3, e infine si rivolge a
%% atomic_list_concat/3 per effettuare una piccola modifica su JSON_String
%% che da stringa Prolog diventa un atomo contenente una stringa.

is_string(String ,JSON_string) :-
    var(String),
    nonvar(JSON_string),
    string(JSON_string),
    atom_string(Atom, JSON_string),
    atom_chars(Atom, Chars),
    is_anychar(Chars, [], _),
    atomic_list_concat([ '\"', JSON_string, '\"'], '', String).

/*********IS*ANYCHAR*(SUPPORTO*A*STRING)*****************************/

%% is_anychar/1 si assicura che il suo argomento non sia nè un singolo
%% nè un doppio apice, dopodichè verifica se è un "graph" secondo le
%% regole di Prolog tramite il predicato char_type/2; sono accettati
%% da is_anychar/1 anche lo spazio bianco, il "tab" e il "newline".

is_anychar(H) :- \+(H = '\"'), \+(H = '\''), char_type(H, graph).
is_anychar(' ').
is_anychar('\n').
is_anychar('\t').

%% Il funzionamento di is_anychar/3 è molto simile a quello
%% di is_integer/3, cioè prevede l'analisi di tutti gli elementi
%% del primo argomento finchè non vengono esaminati tutti
%% (lista vuota, caso base) o non viene trovato un carattere che non
%% unifica con is_anychar/1, tuttavia rispetto a is_integer/3 vi è
%% una definizione in più, in cui vengono accettati i singoli e i doppi
%% apici soltanto se sono immediatamente preceduti da un carattere che
%% unifica con il predicato is_backslash/1.

is_anychar([], [], []) :- !.

%Esc sta per Escape Character
is_anychar([Esc, Quote | Codes], Remains, [Esc, Quote | Already]) :-
    is_backslash(Esc),
    is_quote(Quote),
    is_anychar(Codes, Remains, Already), !.

is_anychar([H | Codes], Remains, [H | Already]) :-
    is_anychar(H),
    is_anychar(Codes, Remains, Already), !.

is_anychar([H | Remains], [H | Remains], []) :-
    \+(is_anychar(H)), !.

/**************************JSONGET*****************************/

%% check_fields/1 si assicura ricorsivamente che gli elementi del suo
%% argomento (una lista) non siano sottoliste e che unifichino con atomic/1.

check_fields([]) :- !.

check_fields([Field | OtherFields]) :-
    \+(is_list(Field)),
    atomic(Field), !,
    check_fields(OtherFields).

%% search_array/3 preleva e restituisce l'elemento (Res) alla posizione
%% Position della lista di jsonarray(); se tale lista è vuota allora
%% restituisce una lista vuota.

search_array(jsonarray([]), _, []) :- !.

search_array(jsonarray(List), Position, Res) :-
    nth0(Position, List, Res).

%% search_obj/3, dopo aver verificato che il secondo argomento Atom
%% unifichi con atom/1, esamina tutte le Pairs contenute nella lista
%% di jsonobj() finchè non ne trova una avente Atom come Attribute;
%% se tale lista è vuota allora restituisce una lista vuota.

search_obj(jsonobj([]), _, []) :- !.

search_obj(jsonobj([Pair | _]), Atom, Res) :-
    atom(Atom),
    search_pair(Pair, Atom, Res).

search_obj(jsonobj([Pair | OtherPair]), Atom, Res) :-
    \+( search_pair(Pair, Atom, Res) ),
    search_obj(jsonobj(OtherPair), Atom, Res).


%% search_pair/3 verifica se la Pair ricevuta ha Atom come Attribute;
%% se sì restituisce il suo Value.

search_pair( (Atom, Value), Atom, Value).


%% search/3 ha due definizioni: nella prima, se è stato riscontrato che
%% JsonObj è un Object, viene ceduto immediatamente il controllo a
%% search_obj/3 mentre nella seconda viene verificato che JsonObj sia
%% un Array e che Field sia un intero non negativo dopodichè viene ceduto
%% il controllo a search_array/3.

search(JsonObj, Field, Ris) :-
    JsonObj = jsonobj(_),
    search_obj(JsonObj, Field, Ris).

search(JsonObj, Field, Ris) :-
    JsonObj = jsonarray(_),
    integer(Field),
    Field >= 0,
    search_array(JsonObj, Field, Ris).


%% is_json_element/1 verifica che il suo argomento sia un Object o un Array.

is_json_element(jsonobj(_)) :- !.
is_json_element(jsonarray(_)) :- !.


%% next_field/3 ha tre definizioni; nella prima viene gestito il caso semplice
%% ma importante in cui la lista dei campi da analizzare in JsonObj è vuota
%% e in tal caso la query principale fallisce. Nella seconda definizione viene
%% gestito il caso in cui la lista dei campi contiene più elementi e per prima
%% cosa vengono invocati search/3 e is_json_element/1, poi viene verificato che
%% il resto della lista (OtherFields) non sia vuota e per concludere viene
%% nuovamente invocato search/3 con Res come primo argomento. Infine
%% nell'ultima definizione viene gestito il caso in cui la lista dei campi
%% è composta da un solo elemento.

next_field(_, [], _).

next_field(JsonObj, [Field | OtherFields], ResExpected) :-
    search(JsonObj, Field, Res),
    is_json_element(Res),
    \+(is_list_empty(OtherFields)),
    next_field(Res, OtherFields, ResExpected).

next_field(JsonObj, [Field | OtherFields], ResExpected) :-
    search(JsonObj, Field, ResExpected),
    is_list_empty(OtherFields).


%% jsonget/3 ha quattro definizioni, ma per la comprensione del
%% funzionamento di tale predicato è sufficiente spiegare soltanto
%% la seconda. Dati in ingresso un oggetto JSON (JSON_obj) e Fields,
%% che può essere un atomo Prolog (prima definizione) o una lista
%% (seconda definizione), jsonget/3 verifica la correttezza sintattica
%% del primo elemento (jsonparse/2) e la correttezza "logica" del
%% secondo (check_fields/1, Fields deve rispettare i vincoli specificati
%% nella consegna), poi cede il controllo a next_field/3 che proverà a
%% trovare il Value indicato da Fields: se non lo avrà trovato, restituirà
%% una lista vuota che farà fallire la query tramite la chiamata
%% a is_list_empty/1. La terza e la quarta definizione si comportano come
%% le prime due, con la differenza che Result non è una variabile Prolog.

jsonget(JSON_obj, Fields, Result) :-
    nonvar(JSON_obj),
    var(Result),
    atomic(Fields),
    jsonget(JSON_obj, [Fields], Result),
    !.

jsonget(JSON_obj, Fields, Result) :-
    nonvar(JSON_obj),
    var(Result),
    jsonparse(_, JSON_obj), !,
    check_fields(Fields), !,
    next_field(JSON_obj, Fields, Result), !,
    \+(is_list_empty(Result)).

jsonget(JSON_obj, Fields, Result) :-
    nonvar(JSON_obj),
    nonvar(Result),
    atomic(Fields),
    jsonget(JSON_obj, [Fields], Result),
    !.

jsonget(JSON_obj, Fields, ResultExp) :-
    nonvar(JSON_obj),
    nonvar(ResultExp),
    jsonparse(_, JSON_obj), !,
    check_fields(Fields), !,
    next_field(JSON_obj, Fields, Result), !,
    ResultExp = Result.

/***********************JSONWRITE*****************************/

%% write_to_file/2 crea il file FileName o lo apre se è già
%% stato creato, scrive Object al suo interno e lo chiude subito dopo.

write_to_file(FileName, Object) :-
    access_file(FileName, write),
    !,
    open(FileName,  write, Stream),
    write(Stream, Object),
    close(Stream).


%% jsonwrite/2 si occupa semplicemente di verificare che i suoi argomenti
%% non siano della variabili, dopodichè chiede aiuto a jsonparse/2 per
%% ricavare la rappresentazione atomica di JSON e inserirla in FileName
%% tramite la chiamata a write_to_file/2.

jsonwrite(JSON, FileName) :-
    nonvar(JSON),
    nonvar(FileName),
    jsonparse(StringJSON , JSON),
    !,
    write_to_file(FileName, StringJSON).

/***********************JSONLOAD*****************************/

%% read_from_file/2 verifica se esiste il file FileName, in tal caso
%% lo apre, ne estrae il contenuto sotto forma di stringa che viene
%% convertita in atomo subito dopo (Atom) e chiude il file.

read_from_file(FileName, Atom) :-
    access_file(FileName, read),
    !,
    open(FileName, read, Stream),
    read_string(Stream, _, String),
    atom_string(Atom, String),
    close(Stream).


%% jsonload/2 si assicura che FileName (Il file dal quale estrarre
%% l'elemento JSON) non sia una variabile, dopodichè invoca read_to_file/2
%% per ricavare l'elemento JSON sotto forma di atomo che verrà poi passato
%% a jsonparse/2.

jsonload(FileName, JSON) :-
    nonvar(FileName),
    read_from_file(FileName, String),
    jsonparse(String, JSON).
