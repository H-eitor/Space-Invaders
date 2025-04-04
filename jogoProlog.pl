:- use_module(library(pce)).
:- use_module(library(time)).

:- dynamic player/1, bullets/1, enemies/1, enemy_bullets/1, enemy_direction/1, timer/1, shields/1.

start :- 
    new(Window, picture('Space Invaders')),
    send(Window, background, black),
    send(Window, size, size(800, 600)),
    send(Window, scrollbars, none),
    send(Window, open_centered),

    % Cria o jogador
    new(Player, box(70, 20)),
    send(Player, fill_pattern, colour(red)),
    send(Player, move, point(360, 550)),
    send(Window, display, Player),
    retractall(player(_)),
    assert(player(Player)),

    % Inicializa as listas e variáveis de estado
    retractall(bullets(_)),
    retractall(enemies(_)),
    retractall(enemy_bullets(_)),
    retractall(enemy_direction(_)),
    retractall(shields(_)),
    assert(bullets([])),
    assert(enemies([])),
    assert(enemy_bullets([])),
    assert(enemy_direction(right)),
    assert(shields([])), % Lista de escudos

    % Cria os escudos de proteção
    create_shields(Window),

    % Cria os inimigos
    create_enemies(Window),

    % Timers para atualização do jogo
    new(BulletTimer, timer(0.01, message(@prolog, update_bullets, Window))),
    send(BulletTimer, start),
    
    new(EnemyTimer, timer(0.05, message(@prolog, move_enemies, Window))),
    send(EnemyTimer, start),
    
    % Timer para inimigos atirarem
    new(EnemyShootTimer, timer(1.5, message(@prolog, enemy_shoot, Window))),
    send(EnemyShootTimer, start),
    
    % Timer para atualizar balas dos inimigos
    new(EnemyBulletTimer, timer(0.01, message(@prolog, update_enemy_bullets, Window))),
    send(EnemyBulletTimer, start),

    % Configura controles do teclado
    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, 'cursor_left', message(@prolog, move_left, Player, Window)),
    send(K, function, 'cursor_right', message(@prolog, move_right, Player, Window)),
    send(K, function, 'SPC', message(@prolog, player_shoot, Player, Window)).

game_over(Window) :-
    % Para todos os timers
    forall(current_timer(Timer), send(Timer, stop)),
    
    % Remove todos os objetos visíveis
    send(Window, clear),  % Limpa toda a janela
    
    % Remove todos os fatos dinâmicos
    retractall(bullets(_)),
    retractall(enemies(_)),
    retractall(enemy_bullets(_)),
    retractall(enemy_direction(_)),
    retractall(player(_)),
    
    % Cria mensagem de game over em uma tela limpa
    new(Text, text('GAME OVER')),
    send(Text, font, font(arial, bold, 36)),
    send(Text, colour, red),
    get(Window, size, size(W, H)),
    TextX is W/2 - 100,
    TextY is H/2 - 18,
    send(Window, display, Text, point(TextX, TextY)),
    send(Window, flush).

create_shields(Window) :-
    retractall(shields(_)),
    assert(shields([])),
    
    % Parâmetros dos escudos
    ShieldWidth is 100,       % Largura de cada escudo
    ShieldHeight is 14,       % Altura aumentada
    HorizontalSpacing is 55,  % Espaço entre colunas
    StartY is 420,            % Posição Y inicial
    
    % Calcula posição X inicial para centralizar
    TotalWidth is (4*ShieldWidth) + (3*HorizontalSpacing),
    StartX is (660 - TotalWidth) // 2,
    
    % Cria 5 colunas de escudos
    between(0, 4, Col),
    % Cria 4 linhas empilhadas (sem espaçamento vertical)
    between(0, 3, Row),
    
    % Calcula posição (sem espaçamento vertical)
    PosX is StartX + Col*(ShieldWidth + HorizontalSpacing),
    PosY is StartY + Row*ShieldHeight,  % Linhas coladas
    
    % Cria cada bloco do escudo
    new(Shield, box(ShieldWidth, ShieldHeight)),
    send(Shield, fill_pattern, colour(cyan)),
    send(Shield, pen, 0),  % Remove a borda para parecer contínuo
    send(Shield, move, point(PosX, PosY)),
    send(Window, display, Shield),
    
    % Adiciona à lista de escudos
    retract(shields(Shields)),
    assert(shields([Shield|Shields])),
    fail.
create_shields(_).

current_timer(Timer) :-
    timer(Timer),
    object(Timer).

% Cria uma fileira de 5 inimigos
create_enemies(Window) :-
    retractall(enemies(_)),
    assert(enemies([])),
    between(1, 5, N),
    X is 150 + (N * 100),
    new(Enemy, box(50, 30)),
    send(Enemy, fill_pattern, colour(green)),
    send(Enemy, move, point(X, 100)),
    send(Window, display, Enemy),
    retract(enemies(Enemies)),
    assert(enemies([Enemy|Enemies])),
    fail.
create_enemies(_).

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
    new(Bullet, box(3, 15)),
    send(Bullet, fill_pattern, colour(blue)),
    BulletX is X + EWidth/2 - 1.5,  % Centraliza o tiro
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
                
                % Remove o jogador
                free(Player),
                retractall(player(_)),
                
                % Chama game over
                game_over(Window)
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


check_shield_collision(BX, BY, BW, BH, [Shield|Rest], Window) :-
    (object(Shield), object(Window) ->
        get(Shield, position, point(SX, SY)),
        get(Shield, width, SW),
        get(Shield, height, SH),
        
        (collision(BX, BY, BW, BH, SX, SY, SW, SH) ->
            % Remove o escudo atingido
            retract(shields(Shields)),
            select(Shield, Shields, NewShields),
            assert(shields(NewShields)),
            free(Shield),
            send(Window, redraw),
            true
        ;
            check_shield_collision(BX, BY, BW, BH, Rest, Window)
        )
    ;
        check_shield_collision(BX, BY, BW, BH, Rest, Window)
    ).
check_shield_collision(_, _, _, _, [], _) :- fail.

move_enemies(Window) :-
    object(Window),  % Verifica se a janela ainda existe
    enemy_direction(Direction),
    enemies(Enemies),
    get(Window, width, WindowWidth),
    move_enemies_list(Enemies, Direction, WindowWidth, Window),
    check_enemies_bounds(Enemies, WindowWidth).  % Passa ambos os argumentos

% Move cada inimigo individualmente
move_enemies_list([], _, _, _).
move_enemies_list([Enemy|Rest], Direction, WindowWidth, Window) :-
    (object(Enemy), object(Window) ->
        get(Enemy, position, point(X, Y)),
        get(Enemy, width, EWidth),
        (Direction == right ->
            NewX is X + 2,
            MaxX is WindowWidth - EWidth,
            (NewX > MaxX -> 
                NewX = MaxX
            ; 
                true
            )
        ;
            % Direction == left
            NewX is X - 2,
            (NewX < 0 -> 
                NewX = 0
            ; 
                true
            )
        ),
        send(Enemy, move, point(NewX, Y)),
        move_enemies_list(Rest, Direction, WindowWidth, Window)
    ;
        move_enemies_list(Rest, Direction, WindowWidth, Window)
    ).

% Verifica se os inimigos atingiram os limites da tela e muda a direção
check_enemies_bounds(Enemies, WindowWidth) :-
    (enemy_hit_bound(Enemies, WindowWidth) ->
        retract(enemy_direction(CurrentDir)),
        (CurrentDir == right ->
            assert(enemy_direction(left))
        ;
            assert(enemy_direction(right))
        )
    ;
        true
    ).

% Versão melhorada de enemy_hit_bound
enemy_hit_bound([Enemy|_], WindowWidth) :-
    object(Enemy),
    get(Enemy, position, point(X, _)),
    get(Enemy, width, EWidth),
    (X =< 1 ; X + EWidth >= WindowWidth - 1),  % Margem de 1 pixel
    !.
enemy_hit_bound([_|Rest], WindowWidth) :-
    enemy_hit_bound(Rest, WindowWidth).
enemy_hit_bound([], _) :- fail.

% Movimento do jogador para a direita com limite
move_right(Box, Window) :-
    object(Window),
    object(Box),
    get(Box, position, point(X, Y)),
    get(Box, width, Width),
    get(Window, width, WindowWidth),
    MaxX is WindowWidth - Width,
    NewX is min(X + 5, MaxX),
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

% Movimento do jogador para a esquerda com limite
move_left(Box, Window) :-
    object(Window),
    object(Box),
    get(Box, position, point(X, Y)),
    NewX is max(X - 5, 0),
    send(Box, move, point(NewX, Y)),
    send(Window, display, Box).

% Jogador atira
player_shoot(Player, Window) :-
    object(Window),
    object(Player),
    get(Player, position, point(X, Y)),
    new(Bullet, box(3, 15)),
    send(Bullet, fill_pattern, colour(yellow)),
    NewX is X + 35,
    NewY is Y - 20,
    send(Bullet, move, point(NewX, NewY)),
    send(Window, display, Bullet),
    % Adiciona bala à lista de balas ativas
    retract(bullets(Bullets)),
    assert(bullets([Bullet | Bullets])).

% Atualiza a posição de todas as balas
update_bullets(Window) :-
    object(Window),
    bullets(Bullets),
    move_bullets(Bullets, Window),
    check_collisions(Window).

% Move as balas e remove as que saíram da tela
move_bullets([], _).
move_bullets([Bullet | Rest], Window) :-
    (object(Bullet), object(Window) ->
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
        % Se o objeto bala não existe mais, remove da lista
        retract(bullets(Bullets)),
        select(Bullet, Bullets, NewBullets),
        assert(bullets(NewBullets)),
        move_bullets(Rest, Window)
    ).

% Verifica colisões entre balas e inimigos
check_collisions(Window) :-
    object(Window),
    bullets(Bullets),
    enemies(Enemies),
    check_bullet_enemy_collisions(Bullets, Enemies, Window).

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

check_player_hit(Window, Bullets) :-
    object(Window),
    findall(B, (member(B, Bullets), object(B)), ExistingBullets),
    retractall(enemy_bullets(_)),
    assert(enemy_bullets(ExistingBullets)).

check_enemy_hit(_, _, _, _, _, [], _).
check_enemy_hit(BX, BY, BW, BH, Bullet, [Enemy|Rest], Window) :-
    (object(Enemy), object(Bullet), object(Window) ->
        get(Enemy, position, point(EX, EY)),
        get(Enemy, width, EW),
        get(Enemy, height, EH),
        (collision(BX, BY, BW, BH, EX, EY, EW, EH) ->
            % Remove a bala e o inimigo
            free(Bullet),
            free(Enemy),
            % Atualiza as listas
            retract(bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(bullets(NewBullets)),
            retract(enemies(Enemies)),
            select(Enemy, Enemies, NewEnemies),
            assert(enemies(NewEnemies)),
            send(Window, redraw)
        ;
            check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
        )
    ;
        check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
    ).

% Verifica se há colisão entre dois retângulos
collision(X1, Y1, W1, H1, X2, Y2, W2, H2) :-
    X1 < X2 + W2,
    X1 + W1 > X2,
    Y1 < Y2 + H2,
    Y1 + H1 > Y2.
