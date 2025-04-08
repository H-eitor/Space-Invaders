check_shield_collision(BX, BY, BW, BH, Shields, Window) :-
    member(Shield, Shields),
    object(Shield),
    object(Window),
    get(Shield, position, point(SX, SY)),
    get(Shield, width, SW),
    get(Shield, height, SH),
    collision(BX, BY, BW, BH, SX, SY, SW, SH),
    % Remove o escudo atingido
    retract(shields(CurrentShields)),
    select(Shield, CurrentShields, NewShields),
    assert(shields(NewShields)),
    free(Shield),
    send(Window, redraw),
    !.  % Corta para evitar verificar outros escudos
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
