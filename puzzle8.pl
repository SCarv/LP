%Sergio Carvalho, ist181513
%Jerzy Pinheiro,  ist182263


dimensao(3).

resolve_manual(Tab_ini, Tab_fin) :- transformacao(Tab_ini, Tab_fin), resolve_manual_(Tab_ini, Tab_fin), !.

resolve_manual_(Tab_fin, Tab_fin) :- writeln('Parabens!').

resolve_manual_(Tab_ini, Tab_fin) :- writeln('Qual o seu movimento?'), 

                                      read(Jogada),

                                      (
                                            mov_legal(Tab_ini, Jogada, _, Tab_next) -> (escreve(Tab_next), nl)
                                            ;
                                            writeln('Movimento ilegal'), Tab_next = Tab_ini
                                      ),

                                      resolve_manual_(Tab_next, Tab_fin).

resolve_cego(Tab_ini, Tab_fin) :- transformacao(Tab_ini, Tab_fin), resolve_cego(Tab_ini, Tab_fin, [], []), !.


resolve_cego(Tab_fin, Tab_fin, _, Movs_inv) :- reverse(Movs_inv, Movs), escreve_solucao(Movs).


resolve_cego(Tab_ini, Tab_fin, Tabs_ants, Movs_inv ) :- mov_legal(Tab_ini, c, Peca, Tab_next),

                                                       (\+member(Tab_next, Tabs_ants) ->

                                                       resolve_cego(Tab_next, Tab_fin, [Tab_ini | Tabs_ants], [(c, Peca) | Movs_inv]));

                                                       mov_legal(Tab_ini, b, Peca, Tab_next),

                                                       (\+member(Tab_next, Tabs_ants) ->

                                                       resolve_cego(Tab_next, Tab_fin, [Tab_ini | Tabs_ants], [(b, Peca) | Movs_inv]));

                                                       mov_legal(Tab_ini, e, Peca, Tab_next),

                                                       (\+member(Tab_next, Tabs_ants) ->

                                                       resolve_cego(Tab_next, Tab_fin, [Tab_ini | Tabs_ants], [(e, Peca) | Movs_inv]));

                                                       mov_legal(Tab_ini, d, Peca, Tab_next),

                                                       (\+member(Tab_next, Tabs_ants) ->

                                                       resolve_cego(Tab_next, Tab_fin, [Tab_ini | Tabs_ants], [(d, Peca) | Movs_inv])).
                                           

constroi_lista_Fs([], []).

constroi_lista_Fs([[_,F,_,_,_]|Abertos], [F | Lista_Fs]) :- constroi_lista_Fs(Abertos, Lista_Fs).

resolve_info_h(Tab_ini, Tab_fin) :- transformacao(Tab_ini, Tab_fin),
                                    
                                    distancia_hamming(Tab_ini, Tab_fin, H_ini),

                                    No_inicial = [Tab_ini, H_ini, 0, H_ini, []], !,

                                    resolve_info_h(No_inicial, Tab_fin, [No_inicial], []), !.

resolve_info_h([Tab_fin, _, _, _,Inv_Solucao], Tab_fin, _, _) :- reverse(Inv_Solucao, Solucao), escreve_solucao(Solucao).

resolve_info_h(_,_,[],_) :- writeln('Nao existe resolucao'), fail.

resolve_info_h([C,F,G,H,M], Tab_fin, Abertos, Fechados) :- delete(Abertos, [C,F,G,H,M], Novo_Abertos1),  %instancia Novo_Abertos1

                                                           expande([C,F,G,H,M], Tab_fin, Sucessores),  %instancia os Sucessores

                                                           subtract(Sucessores, Fechados, Sucessores_semrep),

                                                           union(Sucessores_semrep, Novo_Abertos1, Novo_Abertos2),
 
                                                           escolhe_menor_F(Novo_Abertos2, No_menor_F),

                                                           resolve_info_h(No_menor_F, Tab_fin, Novo_Abertos2, [[C,F,G,H,M]|Fechados]).
                                                           
escolhe_menor_F(Abertos, No_menor_F) :- constroi_lista_Fs(Abertos, Lista_Fs),
                                        min_list(Lista_Fs, Menor_F), %instancia Menor_F
                                        nth1(Indice_menor, Lista_Fs, Menor_F), %instancia Indice_menor
                                        nth1(Indice_menor, Abertos, No_menor_F). %instancia e devolve No_menor_F
                                                           

expande([C,_,G,_,M], Tab_fin, Sucessores) :- G1 is G+1,
                                             findall([Tab_suc, F, G1, H, [(Mov, Peca)| M]], mov_legal(C, Mov, Peca, Tab_suc, Tab_fin, F, G1,H), Sucessores).

mov_legal(Tabuleiro1, Mov, Peca, Tabuleiro2, Tab_final, F, G1, H) :- mov_legal(Tabuleiro1, Mov, Peca, Tabuleiro2),
                                                                     distancia_hamming(Tabuleiro1, Tab_final, H),
                                                                     F is H+G1.
                                                                                            
distancia_hamming([], [], 0).

distancia_hamming([ E1 | Tab_ini], [ E2 | Tab_fin], Dist) :- E1 =\= E2 -> (distancia_hamming(Tab_ini, Tab_fin, Dist_next), Dist is Dist_next+1);
                                                             distancia_hamming(Tab_ini, Tab_fin, Dist).


%%%PREDICADO MOV_LEGAL/4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% os seguintes predicados de movimento legal sao confusos, 
%%% mas pelo menos resolvem o problema para
%%% qualquer dimensao!


mov_legal(Tabuleiro1, Mov, Peca, Tabuleiro2) :- mov_direita(Tabuleiro1, Mov, Peca, Tabuleiro2);

                                                                        mov_baixo(Tabuleiro1, Mov, Peca, Tabuleiro2);

                                                                        mov_esquerda(Tabuleiro1, Mov, Peca, Tabuleiro2);

                                                                        mov_cima(Tabuleiro1, Mov, Peca, Tabuleiro2).


mov_cima(Tabuleiro1, c, Peca, Tabuleiro2) :- dimensao(Dim),
                                                
                                             nth1(Indice_0, Tabuleiro1, 0),

                                                                 Indice_0 =< Dim*Dim - Dim,

                                                     Indice_Peca is Indice_0 + Dim,

                                                     nth1(Indice_Peca, Tabuleiro1, Peca, Resto1),

                                             nth1(Indice_Peca, Tabuleiro2, 0, Resto2),

                                             selectchk(0, Resto1, Peca, Resto2).

                                                

mov_baixo(Tabuleiro1, b, Peca, Tabuleiro2) :- dimensao(Dim),

                                              nth1(Indice_0, Tabuleiro1, 0),

                                                                  Indice_0 > Dim,

                                                                  Indice_Peca is Indice_0 - Dim,

                                                                  nth1(Indice_Peca, Tabuleiro1, Peca, Resto1),

                                                          nth1(Indice_Peca, Tabuleiro2, 0, Resto2),

                                                          selectchk(0, Resto1, Peca, Resto2).


mov_esquerda(Tabuleiro1, e, Peca, Tabuleiro2) :- dimensao(Dim),
                                       
                                                 nth1(Indice_0, Tabuleiro1, 0), %instancia Indice_0 e Resto1

                                                                         Indice_0 mod Dim =\= 0, %nao pode ocorrer translineacao

                                                                         Indice_Peca is Indice_0 + 1, %instancia Indice_Peca

                                                                         nth1(Indice_Peca, Tabuleiro1, Peca, Resto), %instancia P e Resto

                                                                         nth1(Indice_0, Tabuleiro2, Peca, Resto).


mov_direita(Tabuleiro1, d, Peca, Tabuleiro2) :- dimensao(Dim),

                                                nth1(Indice_0, Tabuleiro1, 0), %instancia Indice_0

                                                                        Indice_0 mod Dim =\= 1, %nao pode estar na primeira coluna

                                                                        Indice_Peca is Indice_0 - 1, %instancia Indice_Peca

                                                                        nth1(Indice_Peca, Tabuleiro1, Peca, Resto), %instancia P e Resto

                                                                        nth1(Indice_0, Tabuleiro2, Peca , Resto). %verifica se os tabuleiros sao iguais

%%% transformacao/2
%%% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas
%%% por exemplo
%%% ?- transformacao([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
%%% Transformacao desejada:
%%%  1  2  3      1     2 
%%%  4  5  6  ->  4  5  3 
%%%  7  8         7  8  6 
%%% true .

transformacao([A, B, C, D, E, F, G, H, I], 
              [J, K, L, M, N, O, P, Q, R]) :- write('Transformacao desejada:'), nl, 
                                              escreve(A), escreve(B), escreve(C),  
                                              write('    '), 
                                              escreve(J), escreve(K), escreve(L),nl, 
                                              escreve(D), escreve(E), escreve(F), 
                                              write(' -> '), 
                                              escreve(M), escreve(N), escreve(O), nl,
                                              escreve(G), escreve(H), escreve(I), 
                                              write('    '), 
                                              escreve(P), escreve(Q), escreve(R), nl.


%%% escreve/1 e um predicado auxiliar de transformacao/2
%%% a primeira regra permite escrever uma configuracao
%%% por exemplo
%%% ?- escreve([1, 2, 3, 4, 5, 6, 7, 8, 0]).
%%%  1  2  3 
%%%  4  5  6 
%%%  7  8    
%%% true .

escreve([A, B, C, D, E, F, G, H, I]) :- nl, escreve(A), escreve(B), escreve(C), nl,
                                        escreve(D), escreve(E), escreve(F), nl,
                                        escreve(G), escreve(H), escreve(I), nl.

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').


%%% escreve_solucao/1
%%% escreve_solucao(M) em que M e uma lista de movimentos e um movimento e um par (Mov, Peca) 
%%% por exemplo
%%% ?- escreve_solucao([(b, 6), (b, 3), (d, 2)]).
%%% mova a peca 6 para baixo
%%% mova a peca 3 para baixo
%%% mova a peca 2 para a direita.
%%% true .

escreve_solucao([(M, P) | []]) :- write('mova a peca '), 
                                  write(P), 
                                  traduz(M, Mp), 
                                  write(Mp),
                                  write('.'),
                                  nl.   

escreve_solucao([(M, P) | R]) :- write('mova a peca '), 
                                 write(P), 
                                 traduz(M, Mp), 
                                 write(Mp),
                                 nl, 
                                 escreve_solucao(R).


%%% traduz/2 e um predicado auxiliar de escreve_solucao/1

traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').
