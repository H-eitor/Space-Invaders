  :-include('shield.pl').
% Cria uma fileira de 5 inimigos
create_enemies(Window) :-
    current_phase(Phase),
    (Phase = 5 -> 
        create_boss(Window)  % Fase de chefe
    ;
        create_normal_enemies(Window, Phase)
    ).

create_normal_enemies(Window, Phase) :-
    retractall(enemies(_)),
    assert(enemies([])),
    Rows is min(Phase, 4),  % Máximo de 4 fileiras antes do chefe
    between(1, Rows, Row),
    between(1, 5, Col),  % 5 inimigos por fileira
    X is 150 + (Col * 100),
    Y is 50 + (Row * 50),  % Espaçamento vertical
    new(Enemy, box(50, 30)),
    send(Enemy, fill_pattern, colour(red)),
    send(Enemy, move, point(X, Y)),
    send(Window, display, Enemy),
    retract(enemies(Enemies)),
    assert(enemies([Enemy|Enemies])),
    fail.
create_normal_enemies(_, _).

create_boss(Window) :-
    retractall(enemies(_)),
    retractall(boss_health(_)),
    assert(boss_health(10)),
    
    % Boss é maior e de cor diferente, mas se move igual
    new(Boss, box(100, 50)),  % Tamanho maior que inimigos normais
    send(Boss, fill_pattern, colour(purple)),  % Cor diferente
    send(Boss, move, point(350, 100)),
    send(Window, display, Boss),
    assert(enemies([Boss])).

enemy_shoot(Window) :-
    current_phase(5),
    enemies([Boss]),
    object(Boss),
    boss_health(Health),
    Health > 0, % Só atira se ainda estiver vivo
    get(Boss, position, point(X, Y)),
    get(Boss, width, EWidth),
    
    % Padrão de tiro mais complexo
    forall(between(1, 3, _), (  % 3 tiros de cada vez
        new(Bullet, box(8, 25)),  % Balas maiores
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
    % Escolhe um inimigo aleatório para atirar
    random_member(Shooter, Enemies),
    object(Shooter),
    get(Shooter, position, point(X, Y)),
    get(Shooter, width, EWidth),
    new(Bullet, box(4, 15)),
    send(Bullet, fill_pattern, colour(orange)),
    BulletX is X + EWidth/2 - 3,  % Centraliza o tiro
    BulletY is Y + 30,  % Sai da base do inimigo
    send(Bullet, move, point(BulletX, BulletY)),
    send(Window, display, Bullet),
    % Adiciona bala à lista de balas inimigas
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
        
        % Primeiro verifica colisão com escudos
        (check_shield_collision(BX, NewBY, BW, BH, Shields, Window) ->
            % Remove a bala que atingiu o escudo
            retract(enemy_bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(enemy_bullets(NewBullets)),
            free(Bullet),
            move_enemy_bullets(Rest, Window, Player, Shields)
        ;
            % Se não colidiu com escudo, verifica colisão com jogador
            get(Player, position, point(PX, PY)),
            get(Player, width, PW),
            get(Player, height, PH),
            
            (collision(BX, NewBY, BW, BH, PX, PY, PW, PH) ->
                % Remove a bala
                retract(enemy_bullets(Bullets)),
                select(Bullet, Bullets, NewBullets),
                assert(enemy_bullets(NewBullets)),
                free(Bullet),
                
                % Atualiza contador de vidas
                retract(lives(Lives)),
                NewLives is Lives - 1,
                assert(lives(NewLives)),

                lives_display(LivesDisplay),
                send(LivesDisplay, string, string('Vidas: %d', NewLives)),
                
                (NewLives =< 0 ->
                    % Sem vidas restantes - game over
                    free(Player),
                    retractall(player(_)),
                    game_over(Window)
                ;
                    % Ainda tem vidas - reposiciona o jogador
                    get(Player, position, point(PX, _)),
                    send(Player, move, point(PX, 550)),  % Mantém X, reseta Y
                    send(Window, display, Player)
                )
            ;
                % Continua com o movimento normal
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
            % Remove o escudo atingido
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

move_enemies(Window) :-
    % Código original para fases normais
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
        
        % Velocidade igual para todos os inimigos
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
            % Direction == left
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

% Predicado para mover todos os inimigos para baixo periodicamente
move_enemies_down(_) :-
    current_phase(5), !.

move_enemies_down(Window) :-
    object(Window),
    enemies(Enemies),
    move_all_enemies_down(Enemies, Window),
    check_enemies_position(Window).

check_enemies_position(Window) :-
    enemies(Enemies),
    player(Player),
    object(Player),
    get(Player, position, point(_, PY)),
    (member(Enemy, Enemies), object(Enemy) ->
        get(Enemy, position, point(_, EY)),
        (EY >= PY ->  % Se inimigo alcançou ou passou a linha do jogador
            free(Player),
            retractall(player(_)),
            game_over(Window)
        ;
            true
        )
    ;
        true
    ),
    % Verifica também colisão com escudos
    check_enemy_shield_collision(Window).

% Auxiliar para mover cada inimigo individualmente para baixo
move_all_enemies_down([], _).
move_all_enemies_down([Enemy|Rest], Window) :-
    (object(Enemy), object(Window) ->
        get(Enemy, position, point(X, Y)),
        NewY is Y + 30,  % Distância do movimento para baixo (ajuste conforme necessário)
        send(Enemy, move, point(X, NewY)),
        move_all_enemies_down(Rest, Window)
    ;
        move_all_enemies_down(Rest, Window)
    ).

