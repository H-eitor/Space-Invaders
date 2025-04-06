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
