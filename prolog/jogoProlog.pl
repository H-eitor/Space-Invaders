:- use_module(library(pce)).
:- use_module(library(time)).
:- include('shield.pl').
:- include('enemies.pl').
:- include('player.pl').
:- include('score.pl').
:- dynamic player/1, bullets/1, enemies/1, enemy_bullets/1, enemy_direction/1, timer/1, shields/1, lives/1, lives_display/1, last_shot_time/1, shoot_cooldown/1,
current_phase/1, max_phases/1, boss_defeated/0, boss_health/1, enemy_down_timer/1.

start :-
    write('Digite seu nome:'),
    read(UserName),

    new(Window, picture('Space Invaders')),
    send(Window, background, black),
    send(Window, size, size(800, 600)),
    send(Window, scrollbars, none),
    send(Window, open_centered),

    % Inicializa vidas
    retractall(lives(_)),
    assert(lives(3)),

    retractall(current_phase(_)),
    assert(current_phase(1)),
    retractall(max_phases(_)),
    assert(max_phases(5)),
    retractall(boss_defeated),

    new(LivesText, text('Vidas: 3')),
    send(LivesText, font, font(arial, bold, 25)),
    send(LivesText, colour, white),
    send(Window, display, LivesText, point(20, 580)),
    assert(lives_display(LivesText)),

    % TESTE
    retractall(username(_)),
    assert(username(UserName)),

    % Inicializa pontuação
    retractall(score(_)),
    assert(score(0)),

    % Exibe a pontuação na tela
    new(ScoreText, text('Pontos: 0')),
    send(ScoreText, font, font(arial, bold, 25)),
    send(ScoreText, colour, white),
    send(Window, display, ScoreText, point(650, 580)),
    assert(score_display(ScoreText)),

    % Cria o jogador
    new(Player, box(70, 20)),
    send(Player, fill_pattern, colour(green)),
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

    % Inicializa o sistema de cooldown
    retractall(last_shot_time(_)),
    retractall(shoot_cooldown(_)),
    assert(shoot_cooldown(0.8)),  
    assert(last_shot_time(0)),

    % Timers para atualização do jogo
    new(BulletTimer, timer(0.01, message(@prolog, update_bullets, Window))),
    send(BulletTimer, start),
    
    new(EnemyTimer, timer(0.04, message(@prolog, move_enemies, Window))),
    send(EnemyTimer, start),
    
    % Timer para inimigos atirarem
    new(EnemyShootTimer, timer(1.5, message(@prolog, enemy_shoot, Window))),
    send(EnemyShootTimer, start),

    % Corrigido: parêntese fechado corretamente
    new(CollisionTimer, timer(0.1, message(@prolog, check_enemies_position, Window))),
    send(CollisionTimer, start),
    assert(timer(CollisionTimer)),
    
    % Timer para atualizar balas dos inimigos
    new(EnemyBulletTimer, timer(0.01, message(@prolog, update_enemy_bullets, Window))),
    send(EnemyBulletTimer, start),

    current_phase(Phase),
    descida_intervalo(Phase, Interval),
    (Interval > 0 ->
        retractall(enemy_down_timer(_)), % Remove qualquer timer existente
        new(EnemyDownTimer, timer(Interval, message(@prolog, move_enemies_down, Window))),
        send(EnemyDownTimer, start),
        assert(enemy_down_timer(EnemyDownTimer))
    ; true),

    % Configura controles do teclado
    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, 'cursor_left', message(@prolog, move_left, Player, Window)),
    send(K, function, 'cursor_right', message(@prolog, move_right, Player, Window)),
    send(K, function, 'SPC', message(@prolog, player_shoot, Player, Window)),
    send(K, function, '\\e', message(@prolog, game_over, Window)). % TESTE

game_over(Window) :-
    % Para todos os timers
    forall(current_timer(Timer), send(Timer, stop)),
    
    % Remove todos os objetos visíveis
    send(Window, clear),  % Limpa toda a janela
    
    % Salva a pontuação em um arquivo CSV
    score(Score),
    username(UserName),
    save_score_to_csv(UserName, Score),

    % Remove todos os fatos dinâmicos
    retractall(bullets(_)),
    retractall(enemies(_)),
    retractall(enemy_bullets(_)),
    retractall(enemy_direction(_)),
    retractall(player(_)),
    retractall(lives(_)),
    retractall(lives_display(_)),
    retractall(boss_health(_)),
    retractall(score(_)),
    retractall(score_display(_)),

    % Cria mensagem de game over em uma tela limpa
    new(Text, text('GAME OVER')),
    send(Text, font, font(arial, bold, 36)),
    send(Text, colour, red),
    send(Window, display, Text, point(300, 250)),
    send(Window, flush),
    % Mensagem de ESC
    new(Esc, text('Pressione ESC para sair')),
    send(Esc, font, font(arial, bold, 12)),
    send(Esc, colour, red),
    send(Window, display, Esc, point(300, 300)),
    send(Window, flush),
    % Placar
    highscore(Window),
    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, '\\e', message(@prolog, send, Window, destroy)).

current_timer(Timer) :-
    timer(Timer),
    object(Timer).

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
        
        % Verifica colisão com escudos primeiro
        (check_shield_collision(X, NewY, BW, BH, Shields, Window) ->
            % Remove a bala que atingiu o escudo
            retract(bullets(Bullets)),
            select(Bullet, Bullets, NewBullets),
            assert(bullets(NewBullets)),
            free(Bullet),
            move_bullets(Rest, Window, Shields)
        ;
            % Se não colidiu com escudo, continua movimento normal
            (NewY < 0 ->
                retract(bullets(Bullets)),
                select(Bullet, Bullets, NewBullets),
                assert(bullets(NewBullets)),
                free(Bullet)
            ;
                send(Bullet, move, point(X, NewY))
            ),
            move_bullets(Rest, Window, Shields)
        )
    ;
        % Se o objeto bala não existe mais, remove da lista
        retract(bullets(Bullets)),
        select(Bullet, Bullets, NewBullets),
        assert(bullets(NewBullets)),
        move_bullets(Rest, Window, Shields)
    ).

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

check_player_hit(Window, Bullets) :-
    object(Window),
    findall(B, (member(B, Bullets), object(B)), ExistingBullets),
    retractall(enemy_bullets(_)),
    assert(enemy_bullets(ExistingBullets)).

check_enemy_hit(_, _, _, _, _, [], _).

check_enemy_hit(BX, BY, BW, BH, Bullet, [Boss|_], Window) :-
    current_phase(5),
    object(Boss),
    get(Boss, position, point(EX, EY)),
    get(Boss, width, EW),
    get(Boss, height, EH),
    collision(BX, BY, BW, BH, EX, EY, EW, EH),
    !,
    % Remove a bala
    free(Bullet),
    retract(bullets(Bullets)),
    select(Bullet, Bullets, NewBullets),
    assert(bullets(NewBullets)),
    
    % Reduz a vida do chefe
    retract(boss_health(Health)),
    NewHealth is Health - 1,
    assert(boss_health(NewHealth)),
    
    % Efeito visual - muda de cor quando está fraco
    (NewHealth < 4 -> 
        send(Boss, fill_pattern, colour(red)) 
    ; true),
    
    (NewHealth =< 0 ->
        % Chefe derrotado
        free(Boss),
        retract(enemies(_)),
        assert(enemies([])),
        send(Window, redraw),
        check_phase_completion(Window),
        % Aumenta a pontuação
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
            send(Window, redraw),
            % Aumenta a pontuação
            score(CurrentScore),
            NewScore is CurrentScore + 1,
            update_score(NewScore)
        ;
            check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
        )
    ;
        check_enemy_hit(BX, BY, BW, BH, Bullet, Rest, Window)
    ).

check_phase_completion(Window) :-
    enemies(Enemies),
    Enemies == [],
    current_phase(CurrentPhase),
    max_phases(Max),

    % Remove completamente o timer anterior
    (retract(enemy_down_timer(OldTimer)) -> 
        send(OldTimer, stop),
        free(OldTimer)
    ; true),

    shields(Shields),
    forall(member(S, Shields), (object(S) -> free(S) ; true)), % Corrigido aqui
    retractall(shields(_)),
    assert(shields([])),
    create_shields(Window),

    (CurrentPhase < Max ->
        NextPhase is CurrentPhase + 1,
        retract(current_phase(_)),
        assert(current_phase(NextPhase)),
        
        % Cria novo timer apenas se houver intervalo
        descida_intervalo(NextPhase, NewInterval),
        (NewInterval > 0 ->
            new(NewTimer, timer(NewInterval, message(@prolog, move_enemies_down, Window))),
            send(NewTimer, start),
            assert(enemy_down_timer(NewTimer))
        ; true),
        
        create_enemies(Window)
    ;
        % Fase do chefe completada
        retractall(current_phase(_)),
        assert(current_phase(1)),
        descida_intervalo(1, NewInterval),
        new(NewTimer, timer(NewInterval, message(@prolog, move_enemies_down, Window))),
        send(NewTimer, start),
        assert(enemy_down_timer(NewTimer)),
        create_enemies(Window)
    ).

% Verifica se há colisão entre dois retângulos
collision(X1, Y1, W1, H1, X2, Y2, W2, H2) :-
    X1 < X2 + W2,
    X1 + W1 > X2,
    Y1 < Y2 + H2,
    Y1 + H1 > Y2.
