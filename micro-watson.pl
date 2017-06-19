/*Main Menu*/
micro_watson:-
	write_ln('menu'),
	write_ln('1: Novels/Films of the 20th Century'),
	write_ln('2: Novels of the 19th Century'),
	write_ln('3: Novels of the 18th Century'),
	fluff(2), read(X),micro_watson(X).
micro_watson(Input) :-
	(   Input = 1; Input = 'Select 1'; Input = 'Choose 1'), write_ln('Novels/Films of the 20th Century Selected'), ask;
        (   Input = 2; Input = 'Select 2'; Input = 'Choose 2'), write_ln('Under Construction'), micro_watson;
        (   Input = 3; Input = 'Select 3'; Input = 'Choose 3'), write_ln('Under Construction'), micro_watson;
	(   Input = 'exit' ; Input = 'quit'), fail.

/*Post Menu User Interaction*/
fluff(Type):-
	Type=0, write('micro_watson: ');
	Type=1, fluff(0), write_ln('<wait>');
	Type=2, write('user: ').

ask:-
	fluff(0), write_ln('question?'), fluff(1),fluff(2), read(X), exitController(X), intermediateSentence(X), ! ,ask.

exitController(X):-
	\+(X = 'exit'), \+(X='quit');
	x='change mode', !, ask.

intermediateSentence(Sentence):-
	sentence(Sentence);
	true.

show_answer(Subject, Object, Verb):-
	matcher(Subject,Object, Verb, Answer),
	write(('micro_watson: ')), write_ln(Answer).

/*Language Parsing*/
check([H|T],Out):-
        (dwim_match('Give me a plot element from the following book', H);
	dwim_match('Give me a plot element from the following film',H)),
	reverse_matcher(T, Out).

sentence(Sentence):-
	check(Sentence, Title),
	write_ln(Title).
sentence(Sentence):-
	np(Sentence,Noun_Phrase,Rem,Subject),
	vp(Rem,Verb_Phrase,Verb),
	vpO(Rem,_,Object),
	write_ln(sentence(Noun_Phrase,Verb_Phrase)),
	write('     '),write_ln(Noun_Phrase),
	write('     '),write_ln(Verb_Phrase),
	show_answer(Subject, Object, Verb).
/*Noun Phrase*/
np([X|T],np(det(X),NP2),Rem,Out):-
	det(X),
	np2(T,NP2,Rem,Out).
np(Sentence,np(Parse),Rem,Out):-
	np2(Sentence,Parse,Rem,Out).
np(Sentence,np(NP,PP),Rem,Out):-
	np2(Sentence,NP,Rem1,Out),
	pp(Rem1,PP,Rem).
np2([H|T],np2(noun(H)),T,H):- noun(H).
np2([H|T],np(adj(H),Rest),Rem,Out):- adj(H),np2(T,Rest,Rem,Out).

/*adverbs and prepositional*/
av([H|T],av(adverb(H),Parse),Rem):-
	adverb(H),
	np(T,Parse,Rem,_).

pp([H|T],pp(prep(H),Parse),Rem):-
	prep(H),
	np(T,Parse,Rem,_).

/*Verb Phrase*/
vp([H|[]],verb(H),H):-
	verb(H).
vp([H|T],vp(verb(H),RestParse),H):-
	verb(H),
	np(T,RestParse,_,_).
vp([H|[T]],vp(verb(H), verb(T)),H):-
	verb(H),
	verb(T).
vp([H|T],vp(verb(H), RestParse),H):-
	verb(H),
	pp(T,RestParse,_).

vp([H|T],vp(verb(H), RestParse),H):-
	verb(H),
	av(T,RestParse,_).

vpO([H|T],vp(verb(H),RestParse),Obj):- /*get last noun in verb phrase*/
	np(T,RestParse,_,Obj);
	vpO(T,_,Obj).

vpO([H|[]],vp(verb(H),_),H):- /*get last noun in verb phrase*/
	verb(H).

/*Knowledge Base*/
verb(is).
verb(was).
verb(finds).
verb(saves).
verb(destroys).
verb(expelled).
adj(paranoid).
adj(young).
adj(middle_aged).
adj(faithful).
noun(robot).
noun(hobbit).
noun(holden).
noun(marvin).
noun(ring).
noun(valet).
noun(day).
prep(on).
det(the).
det(a).
adverb(quietly).

plot_story([subject(hobbit),object(ring),verb(finds)],'The Hobbit').
plot_story([subject(hobbit),object(ring),verb(destroys)], 'The Lord of the Rings').
plot_story([subject(valet),object(day),verb(saves)], 'Thankyou Jeeves').
plot_story([subject(holden),object(expelled),verb(is)],'The Catcher in the Rye').
plot_story([subject(robot),object(marvin),verb(was)],'The Hitchhikers Guide to the Galaxy').

plot_story_partial(S,O,_, 'The Hobbit - 66% Confidence || The Lord of the Rings - 66% Confidence'):-
	S=hobbit, O=ring.
plot_story_partial(S,O,V,'The Hobbit - 66% Confidence'):-
	S=hobbit, O=ring;
	S=hobbit, V=finds;
	O=ring, V=finds.
plot_story_partial(S,O,V, 'The Lord of the Rings - 66% Confidence'):-
	S=hobbit, O=ring;
	S=hobbit, V=destroys;
	O=ring, V=destroys.
plot_story_partial(S,O,V, 'Thankyou Jeeves - 66% Confidence'):-
	S=valet, O=day;
	S=valet, V=saves;
	O=day, V=saves.
plot_story_partial(S,O,V,'The Catcher in the Rye - 66% Confidence'):-
	S=holden, O=expelled;
	S=holden, V=is;
	O=expelled, V=is.
plot_story_partial(S,O,V,'The Hitchhikers Guide to the Galaxy - 66% Confidence'):-
	S=robot, O=marvin;
	S=robot, V=was;
	O=marvin, V=was.

no_match('no match').
/*try direct match*/
matcher(S, O, V, Title):-
	plot_story([subject(S),object(O),verb(V)],Title).

/*try partial match*/
matcher(S, O, V, Title):-
	plot_story_partial(S,O,V,Title).

/*no matches*/
matcher(_,_,_,Title):-
	no_match(Title).

reverse_matcher([Title|_], Output):-
	plot_story(Output, Title).

/*Andrew Woodward - 2015*/
