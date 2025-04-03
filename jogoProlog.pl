:- use_module(library(pce)).
:- use_module(library(time)).

start :- 
    new(Window, picture('Teste')),
    send(Window, background, black),
    send(Window, size, size(800, 600)),
    send(Window, open),

    new(Player, box(70, 20)),
    send(Player, fill_pattern, colour(red)),
    send(Player, move, point(360, 550)),
    send(Window, display, Player),

    % Create a list of bullets
    assert(bullets([])),

    % Timer para atualização contínua dos tiros
    new(BulletTimer, timer(0.01, message(@prolog, update_bullets, Window))),
    send(BulletTimer, start),

    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, 'cursor_left', message(@prolog, move_left, Player, Window)),
    send(K, function, 'cursor_right', message(@prolog, move_right, Player, Window)),
    send(K, function, 'SPC', message(@prolog, player_shoot, Player, Window)).

% Movimento para a direita com limite
move_right(Box, Window) :-
    get(Box, position, point(X, Y)),
    get(Box, width, Width),
    get(Window, width, WindowWidth),
    MaxX is WindowWidth - Width,
    NewX is min(X + 5, MaxX),  % Não permite passar do limite direito
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

% Movimento para a esquerda com limite
move_left(Box, Window) :-
    get(Box, position, point(X, Y)),
    NewX is max(X - 5, 0),  % Não permite passar do limite esquerdo (0)
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

player_shoot(Player, Window) :-
    get(Player, position, point(X, Y)),
    new(Bullet, box(3, 15)),
    send(Bullet, fill_pattern, colour(yellow)),
    NewX is X + 35,
    NewY is Y - 20,
    send(Bullet, move, point(NewX, NewY)),
    send(Window, display, Bullet),
    % Add bullet to the list of active bullets
    retract(bullets(Bullets)),
    assert(bullets([Bullet | Bullets])).

update_bullets(Window) :-
    bullets(Bullets),
    move_bullets(Bullets, Window).

move_bullets([], _).
move_bullets([Bullet | Rest], Window) :-
    (object(Bullet) ->
        get(Bullet, position, point(X, Y)),
        NewY is Y - 8,
        send(Bullet, move, point(X, NewY)),
        (NewY < 0 ->
            retract(bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(bullets(NewBullets)),
            free(Bullet)
        ;
            true
        ),
        move_bullets(Rest, Window)
    ;
        % Bullet object doesnt exist anymore, remove from list
        retract(bullets(Bullets)),
        select(Bullet, Bullets, NewBullets),
        assert(bullets(NewBullets)),
        move_bullets(Rest, Window)
    ).
