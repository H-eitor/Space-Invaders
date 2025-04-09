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
