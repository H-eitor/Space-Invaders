:- use_module(library(csv)).
:- use_module(library(pairs)).

update_score(NewScore) :-
    retract(score(_)),
    assert(score(NewScore)),
    score_display(ScoreDisplay),
    send(ScoreDisplay, string, string('Pontos: %d', NewScore)).

save_score_to_csv(UserName, Score) :-
    open('scores.csv', append, Stream),
    atomic_list_concat([UserName, Score], ',', Linha),
    write(Stream, Linha),
    nl(Stream),
    close(Stream).

highscore(Window) :-
    csv_read_file('scores.csv', Linhas, [functor(row)]),
    findall(Score-UserName, member(row(UserName, Score), Linhas), Pairs),
    msort(Pairs, Ordenada),
    reverse(Ordenada, OrdenadaDecrescente),
    findall(User-Score, member(Score-User, OrdenadaDecrescente), Highscore),
    take(10, Highscore, Top10),
    display_highscore(Top10, Window, 300, 330).

display_highscore([], _, _, _).
display_highscore([UserName-Score|Rest], Window, X, Y) :-
    atomic_list_concat([UserName, Score], ': ', Texto),
    new(ScoreText, text(Texto)),
    send(ScoreText, font, font(arial, bold, 20)),
    send(ScoreText, colour, white),
    send(Window, display, ScoreText, point(X, Y)),
    NewY is Y + 30,    
    display_highscore(Rest, Window, X, NewY).

take(_,[],[]):-!.
take(0, _, []):-!.
take(N, [Head|Tail], [Head|Result]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Tail, Result).

comparar_pares(Ord, Key1-Value1, Key2-Value2) :-
    ( Key1 = Key2 -> 
        compare(Ord, Value1, Value2)
    ;
        compare(Ord, Key, Key)
    ).