check_shield_collision(BX, BY, BW, BH, Shields, Window) :-
    member(Shield, Shields),
    object(Shield),
    object(Window),
    get(Shield, position, point(SX, SY)),
    get(Shield, width, SW),
    get(Shield, height, SH),
    collision(BX, BY, BW, BH, SX, SY, SW, SH),
    retract(shields(CurrentShields)),
    select(Shield, CurrentShields, NewShields),
    assert(shields(NewShields)),
    free(Shield),
    send(Window, redraw),
    !.  
check_shield_collision(_, _, _, _, _, _) :- fail.

% Verifica colisões entre balas e inimigos
check_collisions(Window) :-
    object(Window),
    bullets(Bullets),
    enemies(Enemies),
    check_bullet_enemy_collisions(Bullets, Enemies, Window),
    (Enemies == [] -> check_phase_completion(Window) ; true).

check_bullet_enemy_collisions([], _, _).
check_bullet_enemy_collisions([Bullet|RestBullets], Enemies, Window) :-
    (object(Bullet), object(Window) ->
        get(Bullet, position, point(BX, BY)),
        get(Bullet, width, BW),
        get(Bullet, height, BH),
        check_enemy_hit(BX, BY, BW, BH, Bullet, Enemies, Window),
        check_bullet_enemy_collisions(RestBullets, Enemies, Window)
    ;
        check_bullet_enemy_collisions(RestBullets, Enemies, Window)
    ).

% Verifica colisao do inimigo com o jogador
check_player_enemy_collision(Window) :-
    player(Player),
    object(Player),
    enemies(Enemies),
    check_player_enemy_collision_list(Player, Enemies, Window).

check_player_enemy_collision_list(_, [], _).
check_player_enemy_collision_list(Player, [Enemy|Rest], Window) :-
    (object(Player), object(Enemy), object(Window) ->
        get(Player, position, point(PX, PY)),
        get(Player, width, PW),
        get(Player, height, PH),
        get(Enemy, position, point(EX, EY)),
        get(Enemy, width, EW),
        get(Enemy, height, EH),
        (collision(PX, PY, PW, PH, EX, EY, EW, EH) ->
            free(Player),
            retractall(player(_)),
            game_over(Window)
        ;
            check_player_enemy_collision_list(Player, Rest, Window)
        )
    ;
        check_player_enemy_collision_list(Player, Rest, Window)
    ).

% Verifica se há colisão entre dois retângulos
collision(X1, Y1, W1, H1, X2, Y2, W2, H2) :-
    X1 < X2 + W2,
    X1 + W1 > X2,
    Y1 < Y2 + H2,
    Y1 + H1 > Y2.
