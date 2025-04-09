% Atualiza a posição de todas as balas
update_bullets(Window) :-
    object(Window),
    bullets(Bullets),
    shields(Shields),
    move_bullets(Bullets, Window, Shields),
    check_collisions(Window).

% Move as balas e remove as que saíram da tela
move_bullets([], _, _).
move_bullets([Bullet | Rest], Window, Shields) :-
    (object(Bullet), object(Window) ->
        get(Bullet, position, point(X, Y)),
        get(Bullet, width, BW),
        get(Bullet, height, BH),
        NewY is Y - 8,
        
        (check_shield_collision(X, NewY, BW, BH, Shields, Window) ->
            % Remove a bala que atingiu o escudo
            retract(bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(bullets(NewBullets)),
            free(Bullet),
            send(Window, redraw)
        ;
            % Continua com o movimento normal
            (NewY < 0 ->
                retract(bullets(Bullets)),
                select(Bullet, Bullets, NewBullets),
                assert(bullets(NewBullets)),
                free(Bullet),
                send(Window, redraw)
            ;
                send(Bullet, move, point(X, NewY))
            )
        ),
        move_bullets(Rest, Window, Shields)
    ;
        % Se o objeto bala não existe mais, remove da lista
        retract(bullets(Bullets)),
        select(Bullet, Bullets, NewBullets),
        assert(bullets(NewBullets)),
        free(Bullet),
        move_bullets(Rest, Window, Shields)
    ).
