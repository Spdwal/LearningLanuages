regins(rhodri, 844, 878).
regins(anarawd, 878, 916).
regins(hywel_dda, 916, 950).
regins(lago_ap_idwal, 950, 979).
regins(hywel_ap_ieuaf, 979, 985).
regins(cadwallon, 985, 986).
regins(maredudd, 986, 999).

prince(X, Y) :-
    regins(X, A, B),
    Y >= A,
    Y =< B.
