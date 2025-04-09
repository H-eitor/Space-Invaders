check_phase_completion(Window) :-
    enemies([]),  % Verifica se não há inimigos
    current_phase(CurrentPhase),
    max_phases(Max),
    object(Window),

    % Remove todos os timers antigos
    (enemy_down_timer(OldMoveTimer), object(OldMoveTimer) -> 
        send(OldMoveTimer, stop),
        free(OldMoveTimer),
        retract(enemy_down_timer(OldMoveTimer))
    ; true),

    (enemy_shoot_timer(OldShootTimer), object(OldShootTimer) -> 
        send(OldShootTimer, stop),
        free(OldShootTimer),
        retract(enemy_shoot_timer(OldShootTimer))
    ; true),

    % Limpa todas as balas do jogador
    (retract(bullets(PlayerBullets)) ->
        clean_bullet_list(PlayerBullets),
        assert(bullets([]))
    ; true),

    % Limpa todas as balas inimigas
    (retract(enemy_bullets(EnemyBullets)) ->
        clean_bullet_list(EnemyBullets),
        assert(enemy_bullets([]))
    ; true),

    % Remove e libera todos os escudos existentes
    (retract(shields(Shields)) ->
        forall(member(Shield, Shields), 
              (object(Shield) -> free(Shield); true)),
        retractall(shields(_))
    ; true),

    % Atualiza para próxima fase ou reinicia
    (CurrentPhase < Max ->
        NextPhase is CurrentPhase + 1,
        retractall(current_phase(_)),
        assert(current_phase(NextPhase))
    ;
        retractall(current_phase(_)),
        assert(current_phase(1))
    ),

    % Configura novos timers para a fase
    current_phase(Phase),
    descida_intervalo(Phase, NewInterval),
    new(NewMoveTimer, timer(NewInterval, message(@prolog, move_enemies_down, Window))),
    send(NewMoveTimer, start),
    assert(enemy_down_timer(NewMoveTimer)),
    
    enemy_shoot_interval(Phase, NewShootInterval),
    new(NewShootTimer, timer(NewShootInterval, message(@prolog, enemy_shoot, Window))),
    send(NewShootTimer, start),
    assert(enemy_shoot_timer(NewShootTimer)),

    % Cria novos escudos
    create_shields(Window),
    
    % Cria inimigos da nova fase
    create_enemies(Window),
    
    % Atualiza a tela
    send(Window, redraw).

% Tela de game over.
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
    send(Window, display, Text, point(250, 200)),
    
    % Mensagem de ESC
    new(Esc, text('Pressione ESC para sair')),
    send(Esc, font, font(arial, bold, 12)),
    send(Esc, colour, red),
    send(Window, display, Esc, point(250, 250)),
    
    % Adiciona o título "TOP 5 PLACARES:"
    new(TopScoresTitle, text('TOP 5 PLACARES REGISTRADOS:')),
    send(TopScoresTitle, font, font(arial, bold, 24)),
    send(TopScoresTitle, colour, yellow),
    send(Window, display, TopScoresTitle, point(250, 300)),
    
    % Exibe os highscores abaixo do título
    highscore(Window, 250, 340),
    
    send(Window, recogniser, new(K, key_binding(@nil, argument))),
    send(K, function, '\\e', message(@prolog, send, Window, destroy)).

