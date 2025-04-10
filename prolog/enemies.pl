:-include('shield.pl').

descida_intervalo(1, 8). 
descida_intervalo(2, 7).    
descida_intervalo(3, 5).    
descida_intervalo(4, 4).    
descida_intervalo(5, 0). 

enemy_shoot_interval(1, 2).  
enemy_shoot_interval(2, 1.6).  
enemy_shoot_interval(3, 1.3).  
enemy_shoot_interval(4, 1.3).  
enemy_shoot_interval(5, 0.8).

% Cria os inimigos
create_enemies(Window) :-
    current_phase(Phase),
    (Phase = 5 -> 
        create_boss(Window) 
    ;
        create_normal_enemies(Window, Phase)
    ).

% Cria inimigos normais
create_normal_enemies(Window, Phase) :-
    retractall(enemies(_)),
    assert(enemies([])),
    Rows is min(Phase, 4),  
    between(1, Rows, Row),
    between(1, 5, Col),  
    X is 150 + (Col * 100),
    Y is 50 + (Row * 50),  
    new(Enemy, box(50, 30)),
    send(Enemy, fill_pattern, colour(red)),
    send(Enemy, move, point(X, Y)),
    send(Window, display, Enemy),
    retract(enemies(Enemies)),
    assert(enemies([Enemy|Enemies])),
    fail.
create_normal_enemies(_, _).

% Cria o chefe
create_boss(Window) :-
    retractall(enemies(_)),
    retractall(boss_health(_)),
    assert(boss_health(10)),
    
    new(Boss, box(100, 50)),  
    send(Boss, fill_pattern, colour(purple)),  
    send(Boss, move, point(350, 100)),
    send(Window, display, Boss),
    assert(enemies([Boss])).

enemy_shoot(Window) :-
    current_phase(5),
    enemies([Boss]),
    object(Boss),
    boss_health(Health),
    Health > 0, 
    get(Boss, position, point(X, Y)),
    get(Boss, width, EWidth),
    
    forall(between(1, 3, _), (  
        new(Bullet, box(8, 25)),  
        send(Bullet, fill_pattern, colour(orange)),
        BulletX is X + random(EWidth) - 3,
        BulletY is Y + 50,
        send(Bullet, move, point(BulletX, BulletY)),
        send(Window, display, Bullet),
        retract(enemy_bullets(Bullets)),
        assert(enemy_bullets([Bullet | Bullets]))
    )).

% Inimigos atiram
enemy_shoot(Window) :-
    object(Window),
    enemies(Enemies),
    Enemies \= [],
    
    random_member(Shooter, Enemies),
    object(Shooter),
    get(Shooter, position, point(X, Y)),
    get(Shooter, width, EWidth),
    new(Bullet, box(4, 15)),
    send(Bullet, fill_pattern, colour(orange)),
    BulletX is X + EWidth/2 - 3,  
    BulletY is Y + 30,  
    send(Bullet, move, point(BulletX, BulletY)),
    send(Window, display, Bullet),
    retract(enemy_bullets(Bullets)),
    assert(enemy_bullets([Bullet | Bullets])).

% Atualiza a posição das balas inimigas
update_enemy_bullets(Window) :-
    object(Window),
    enemy_bullets(Bullets),
    player(Player),
    shields(Shields),
    move_enemy_bullets(Bullets, Window, Player, Shields).

% Move as balas inimigas e remove as que saíram da tela
move_enemy_bullets([], _, _, _).
move_enemy_bullets([Bullet | Rest], Window, Player, Shields) :-
    (object(Bullet), object(Window), object(Player) ->
        get(Bullet, position, point(BX, BY)),
        get(Bullet, width, BW),
        get(Bullet, height, BH),
        NewBY is BY + 5,
        
        (check_shield_collision(BX, NewBY, BW, BH, Shields, Window) ->
            retract(enemy_bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(enemy_bullets(NewBullets)),
            free(Bullet),
            move_enemy_bullets(Rest, Window, Player, Shields)
        ;
            get(Player, position, point(PX, PY)),
            get(Player, width, PW),
            get(Player, height, PH),
            
            (collision(BX, NewBY, BW, BH, PX, PY, PW, PH) ->
                retract(enemy_bullets(Bullets)),
                select(Bullet, Bullets, NewBullets),
                assert(enemy_bullets(NewBullets)),
                free(Bullet),
                
                retract(lives(Lives)),
                NewLives is Lives - 1,
                assert(lives(NewLives)),

                lives_display(LivesDisplay),
                send(LivesDisplay, string, string('Vidas: %d', NewLives)),
                
                (NewLives =< 0 ->
                    free(Player),
                    retractall(player(_)),
                    game_over(Window)
                ;
                    get(Player, position, point(PX, _)),
                    send(Player, move, point(PX, 550)),  
                    send(Window, display, Player)
                )
            ;
                get(Window, height, WindowHeight),
                (NewBY > WindowHeight ->
                    retract(enemy_bullets(Bullets)),
                    select(Bullet, Bullets, NewBullets),
                    assert(enemy_bullets(NewBullets)),
                    free(Bullet),
                    send(Window, redraw)
                ;
                    send(Bullet, move, point(BX, NewBY)),
                    move_enemy_bullets(Rest, Window, Player, Shields)
                )
            )
        )
    ;
        move_enemy_bullets(Rest, Window, Player, Shields)
    ).

%Verifica colisao dos inimigos com escudos
check_enemy_shield_collision(Window) :-
    enemies(Enemies),
    shields(Shields),
    check_enemy_shield_collision_list(Enemies, Shields, Window).

check_enemy_shield_collision_list([], _, _).
check_enemy_shield_collision_list([Enemy|RestEnemies], Shields, Window) :-
    (object(Enemy), object(Window) ->
        get(Enemy, position, point(EX, EY)),
        get(Enemy, width, EW),
        get(Enemy, height, EH),
        check_enemy_with_shields(EX, EY, EW, EH, Shields, Window),
        check_enemy_shield_collision_list(RestEnemies, Shields, Window)
    ;
        check_enemy_shield_collision_list(RestEnemies, Shields, Window)
    ).

check_enemy_with_shields(_, _, _, _, [], _).
check_enemy_with_shields(EX, EY, EW, EH, [Shield|Rest], Window) :-
    (object(Shield), object(Window) ->
        get(Shield, position, point(SX, SY)),
        get(Shield, width, SW),
        get(Shield, height, SH),
        
        (collision(EX, EY, EW, EH, SX, SY, SW, SH) ->
            retract(shields(Shields)),
            select(Shield, Shields, NewShields),
            assert(shields(NewShields)),
            free(Shield),
            send(Window, redraw)
        ;
            check_enemy_with_shields(EX, EY, EW, EH, Rest, Window)
        )
    ;
        check_enemy_with_shields(EX, EY, EW, EH, Rest, Window)
    ).

% Move os inimigos
move_enemies(Window) :-
    object(Window),
    enemy_direction(Direction),
    enemies(Enemies),
    get(Window, width, WindowWidth),
    move_enemies_list(Enemies, Direction, WindowWidth, Window).

move_enemies_list([], _, _, _).
move_enemies_list([Enemy|Rest], Direction, WindowWidth, Window) :-
    (object(Enemy), object(Window) ->
        get(Enemy, position, point(X, Y)),
        get(Enemy, width, EWidth),
        
        Speed = 4,
        
        (Direction == right ->
            NewX is X + Speed,
            MaxX is WindowWidth - EWidth,
            (NewX >= MaxX ->
                retract(enemy_direction(_)),
                assert(enemy_direction(left)),
                NewXFinal is MaxX
            ;
                NewXFinal = NewX
            )
        ;
            NewX is X - Speed,
            (NewX =< 0 ->
                retract(enemy_direction(_)),
                assert(enemy_direction(right)),
                NewXFinal = 0
            ;
                NewXFinal = NewX
            )
        ),
        
        send(Enemy, move, point(NewXFinal, Y)),
        move_enemies_list(Rest, Direction, WindowWidth, Window)
    ;
        move_enemies_list(Rest, Direction, WindowWidth, Window)
    ).

% Move os inimigos para baixo (com exceção da fase 5)
move_enemies_down(_) :-
    current_phase(5), !.

move_enemies_down(Window) :-
    object(Window),
    enemies(Enemies),
    Enemies \= [], 
    once(move_all_enemies_down(Enemies, Window)), 
    check_enemies_position(Window).

% Verifica a posicao dos inimigos
check_enemies_position(Window) :-
    enemies(Enemies),
    player(Player),
    object(Player),
    get(Player, position, point(_, PY)),
    (member(Enemy, Enemies), object(Enemy) ->
        get(Enemy, position, point(_, EY)),
        (EY >= PY -> 
            free(Player),
            retractall(player(_)),
            game_over(Window)
        ;
            true
        )
    ;
        true
    ),
    check_enemy_shield_collision(Window).

move_all_enemies_down([], _).
move_all_enemies_down([Enemy|Rest], Window) :-
    (object(Enemy), object(Window) ->
        get(Enemy, position, point(X, Y)),
        NewY is Y + 30,  
        send(Enemy, move, point(X, NewY)),
        move_all_enemies_down(Rest, Window)
    ;
        move_all_enemies_down(Rest, Window)
    ).

% Verifica se algum inimigo foi atingido
check_enemy_hit(_, _, _, _, _, [], _).
check_enemy_hit(BX, BY, BW, BH, Bullet, [Boss|_], Window) :-
    current_phase(5),
    object(Boss),
    get(Boss, position, point(EX, EY)),
    get(Boss, width, EW),
    get(Boss, height, EH),
    collision(BX, BY, BW, BH, EX, EY, EW, EH),
    !,
    free(Bullet),
    retract(bullets(Bullets)),
    select(Bullet, Bullets, NewBullets),
    assert(bullets(NewBullets)),
    

    retract(boss_health(Health)),
    NewHealth is Health - 1,
    assert(boss_health(NewHealth)),
    
    (NewHealth < 4 -> 
        send(Boss, fill_pattern, colour(red)) 
    ; true),
    
    (NewHealth =< 0 ->
        free(Boss),
        retract(enemies(_)),
        assert(enemies([])),
        send(Window, redraw),
        check_phase_completion(Window),
        score(CurrentScore),
        NewScore is CurrentScore + 30,
        update_score(NewScore)
    ;
        true
    ).

check_enemy_hit(BX, BY, BW, BH, Bullet, [Enemy|Rest], Window) :-
    (object(Enemy), object(Bullet), object(Window) ->
        get(Enemy, position, point(EX, EY)),
        get(Enemy, width, EW),
        get(Enemy, height, EH),
        (collision(BX, BY, BW, BH, EX, EY, EW, EH) ->
            free(Bullet),
            free(Enemy),
            retract(bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(bullets(NewBullets)),
            retract(enemies(Enemies)),
            select(Enemy, Enemies, NewEnemies),
            assert(enemies(NewEnemies)),
            send(Window, redraw),
            score(CurrentScore),
            NewScore is CurrentScore + 1,
            update_score(NewScore)
        ;
            check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
        )
    ;
        check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
    ).
