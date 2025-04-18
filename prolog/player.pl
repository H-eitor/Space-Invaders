% Movimento do jogador para a direita
move_right(Box, Window) :-
    object(Window),
    object(Box),
    get(Box, position, point(X, Y)),
    get(Box, width, Width),
    get(Window, width, WindowWidth),
    MaxX is WindowWidth - Width,
    NewX is min(X + 22, MaxX),
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

% Movimento do jogador para a esquerda
move_left(Box, Window) :-
    object(Window),
    object(Box),
    get(Box, position, point(X, Y)),
    NewX is max(X - 22, 0),
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

% Tiro do jogador
player_shoot(Player, Window) :-
    object(Window),
    object(Player),
    get_time(CurrentTime),
    last_shot_time(LastTime),
    shoot_cooldown(Cooldown),
    (CurrentTime - LastTime >= Cooldown ->
        get(Player, position, point(X, Y)),
        new(Bullet, box(4, 15)),
        send(Bullet, fill_pattern, colour(yellow)),
        NewX is X + 34,
        NewY is Y - 20,
        send(Bullet, move, point(NewX, NewY)),
        send(Window, display, Bullet),
        
        retract(bullets(Bullets)),
        assert(bullets([Bullet | Bullets])),
        
        retractall(last_shot_time(_)),
        assert(last_shot_time(CurrentTime))
    ;
        true
    ).

% Verifica se o jogador foi atingido por uma bala inimiga
check_player_hit(Window, Bullets) :-
    object(Window),
    findall(B, (member(B, Bullets), object(B)), ExistingBullets),
    retractall(enemy_bullets(_)),
    assert(enemy_bullets(ExistingBullets)).
