% Cria os escudos
create_shields(Window) :-
    retractall(shields(_)),
    assert(shields([])),
    
    ShieldWidth is 100,       
    ShieldHeight is 14,       
    HorizontalSpacing is 55,  
    StartY is 420,            
    
    TotalWidth is (4*ShieldWidth) + (3*HorizontalSpacing),
    StartX is (660 - TotalWidth) // 2,
    
    between(0, 4, Col),
    between(0, 3, Row),
    
    PosX is StartX + Col*(ShieldWidth + HorizontalSpacing),
    PosY is StartY + Row*ShieldHeight,  
    
    new(Shield, box(ShieldWidth, ShieldHeight)),
    send(Shield, fill_pattern, colour(cyan)),
    send(Shield, pen, 0),  
    send(Shield, move, point(PosX, PosY)),
    send(Window, display, Shield),
    
    retract(shields(Shields)),
    assert(shields([Shield|Shields])),
    fail.
create_shields(_).
