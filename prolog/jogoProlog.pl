:- use_module(library(pce)).
:- use_module(library(time)).
:- include('shield.pl').
:- include('enemies.pl').
:- include('player.pl').
:- include('score.pl').
:- include('collisions.pl').
:- include('bullet.pl').
:- include('gamePhase.pl').

% Fatos dinamicos do jogo
:- dynamic player/1, bullets/1, enemies/1, enemy_bullets/1, enemy_direction/1, timer/1, shields/1, lives/1, lives_display/1, last_shot_time/1, shoot_cooldown/1,
current_phase/1, max_phases/1, boss_defeated/0, boss_health/1, enemy_down_timer/1, enemy_shoot_timer/1.

start :-
    write('Digite seu nome: '),
    read_string(UserName),

    new(Window, picture('Space Invaders')),
    send(Window, background, black),
    send(Window, size, size(800, 600)),
    send(Window, scrollbars, none),
    send(Window, open_centered),

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

    retractall(username(_)),
    assert(username(UserName)),

    retractall(score(_)),
    assert(score(0)),

    new(ScoreText, text('Pontos: 0')),
    send(ScoreText, font, font(arial, bold, 25)),
    send(ScoreText, colour, white),
    send(Window, display, ScoreText, point(650, 580)),
    assert(score_display(ScoreText)),

    get_highscore(TopUser, TopScore),
    format(atom(HighScoreText), 'Recorde: ~d Pontos, por ~w ', [TopScore, TopUser]),
    new(HighText, text(HighScoreText)),
    send(HighText, font, font(arial, bold, 18)),
    send(HighText, colour, white),
    send(Window, display, HighText, point(280, 10)),

    new(Player, box(70, 20)),
    send(Player, fill_pattern, colour(green)),
    send(Player, move, point(360, 550)),
    send(Window, display, Player),
    retractall(player(_)),
    assert(player(Player)),

    retractall(bullets(_)),
    retractall(enemies(_)),
    retractall(enemy_bullets(_)),
    retractall(enemy_direction(_)),
    retractall(shields(_)),
    assert(bullets([])),
    assert(enemies([])),
    assert(enemy_bullets([])),
    assert(enemy_direction(right)),
    assert(shields([])), 

    create_shields(Window),
    create_enemies(Window),

    retractall(last_shot_time(_)),
    retractall(shoot_cooldown(_)),
    assert(shoot_cooldown(0.8)),  
    assert(last_shot_time(0)),

    new(BulletTimer, timer(0.01, message(@prolog, update_bullets, Window))),
    send(BulletTimer, start),
    
    new(EnemyTimer, timer(0.04, message(@prolog, move_enemies, Window))),
    send(EnemyTimer, start),
    
    current_phase(Phase),
    enemy_shoot_interval(Phase, ShootInterval),
    new(EnemyShootTimer, timer(ShootInterval, message(@prolog, enemy_shoot, Window))),
    send(EnemyShootTimer, start),

    new(CollisionTimer, timer(0.1, message(@prolog, check_enemies_position, Window))),
    send(CollisionTimer, start),
    assert(timer(CollisionTimer)),
    
    new(EnemyBulletTimer, timer(0.01, message(@prolog, update_enemy_bullets, Window))),
    send(EnemyBulletTimer, start),

    current_phase(Phase),
    descida_intervalo(Phase, Interval),
    (Interval > 0 ->
        retractall(enemy_down_timer(_)), 
        new(EnemyDownTimer, timer(Interval, message(@prolog, move_enemies_down, Window))),
        send(EnemyDownTimer, start),
        assert(enemy_down_timer(EnemyDownTimer))
    ; true),

    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, 'cursor_left', message(@prolog, move_left, Player, Window)),
    send(K, function, 'cursor_right', message(@prolog, move_right, Player, Window)),
    send(K, function, 'SPC', message(@prolog, player_shoot, Player, Window)),
    send(K, function, '\\e', message(@prolog, game_over, Window)). 


%Timer atual do jogo
current_timer(Timer) :-
    timer(Timer),
    object(Timer).

read_string(String) :-
   current_input(Input),
   read_line_to_codes(Input, Codes),
   string_codes(String, Codes).

% Limpa a lista de balas
clean_bullet_list([]).
clean_bullet_list([Bullet|Rest]) :-
    (object(Bullet) -> free(Bullet); true),
    clean_bullet_list(Rest).

