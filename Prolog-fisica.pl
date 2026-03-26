% Hechos
mass(ball, 2).
velocity(ball, 5).
accel(ball, 3).
position(ball, 0).

mass(car, 1000).
velocity(car, 20).
accel(car, -2).
position(car, 0).


kinetic_energy(Object, Energy) :-
    mass(Object, M),
    velocity(Object, V),
    Energy is 0.5 * M * V * V.


discriminant(A, B, C, D) :-
    D is B*B - 4*A*C.


impact_time(Object, Distance, Time) :-
    position(Object, X0),
    velocity(Object, V0),
    accel(Object, A),

    A_quad is 0.5 * A,
    B is V0,
    C is X0 - Distance,

    ( A_quad =:= 0 ->
        % Movimiento uniforme
        Time is -C / B
    ;
        discriminant(A_quad, B, C, D),
        D >= 0,
        SqrtD is sqrt(D),

        T1 is (-B + SqrtD) / (2 * A_quad),
        T2 is (-B - SqrtD) / (2 * A_quad),

        % Elegir el menor tiempo positivo
        (T1 >= 0 -> Tpos1 = T1 ; Tpos1 = 1.0e12),
        (T2 >= 0 -> Tpos2 = T2 ; Tpos2 = 1.0e12),

        Time is min(Tpos1, Tpos2)
    ).


safe_system(Object) :-
    kinetic_energy(Object, Energy),
    Energy < 50.


position_at_time(Object, Time, Position) :-
    position(Object, X0),
    velocity(Object, V0),
    accel(Object, A),
    Position is X0 + V0*Time + 0.5*A*Time*Time.


velocity_at_time(Object, Time, Velocity) :-
    velocity(Object, V0),
    accel(Object, A),
    Velocity is V0 + A*Time.


kinetic_energy_at_time(Object, Time, Energy) :-
    mass(Object, M),
    velocity_at_time(Object, Time, V),
    Energy is 0.5 * M * V * V.


energy_list(Object, EndDistance, TimeStep, EnergyList) :-
    energy_list_helper(Object, EndDistance, TimeStep, 0, [], EnergyList).

energy_list_helper(Object, EndDistance, TimeStep, Time, Acc, Result) :-
    position_at_time(Object, Time, Distance),
    ( Distance > EndDistance ->
        reverse(Acc, Result)
    ;
        kinetic_energy_at_time(Object, Time, Energy),
        (Energy < 50 -> Safe = true ; Safe = false),

        NewAcc = [Energy-Safe-Time-Distance | Acc],
        NewTime is Time + TimeStep,

        energy_list_helper(Object, EndDistance, TimeStep, NewTime, NewAcc, Result)
    ).


% Hechos
amplitude(w1, 2).
frequency(w1, 5).
wave_num(w1, 2).

amplitude(w2, 1.5).
frequency(w2, 3).
wave_num(w2, 1.5).

pi(3.141592653589793).


wav_tim_evolution(Wave, StartTime, EndTime, Position, Result) :-
    amplitude(Wave, A),
    frequency(Wave, F),
    wave_num(Wave, K),
    pi(PI),

    AngularFreq is 2 * PI * F,
    Step is 0.1,

    wav_time_helper(StartTime, EndTime, Position, A, K, AngularFreq, Step, [], Result).

wav_time_helper(Time, EndTime, _, _, _, _, _, Acc, Result) :-
    Time > EndTime,
    reverse(Acc, Result).

wav_time_helper(Time, EndTime, Pos, A, K, W, Step, Acc, Result) :-
    Time =< EndTime,
    Value is A * sin(K*Pos - W*Time),

    NewAcc = [Value-Time | Acc],
    NewTime is Time + Step,

    wav_time_helper(NewTime, EndTime, Pos, A, K, W, Step, NewAcc, Result).


wav_spatial_evolution(Wave, StartPos, EndPos, Time, Result) :-
    amplitude(Wave, A),
    frequency(Wave, F),
    wave_num(Wave, K),
    pi(PI),

    AngularFreq is 2 * PI * F,
    Step is 0.1,

    wav_space_helper(StartPos, EndPos, Time, A, K, AngularFreq, Step, [], Result).

wav_space_helper(Pos, EndPos, _, _, _, _, _, Acc, Result) :-
    Pos > EndPos,
    reverse(Acc, Result).

wav_space_helper(Pos, EndPos, Time, A, K, W, Step, Acc, Result) :-
    Pos =< EndPos,
    Value is A * sin(K*Pos - W*Time),

    NewAcc = [Value-Pos | Acc],
    NewPos is Pos + Step,

    wav_space_helper(NewPos, EndPos, Time, A, K, W, Step, NewAcc, Result).