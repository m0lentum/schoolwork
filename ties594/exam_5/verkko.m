function [solmut, elementit] = verkko

% rivillä i on solmuun numero i liittyiä tietoja;
% kaksi ensimmäistä lukua ovat solmun (kaksiulotteiset) koordinaatit ja 
% kolmas kertoo, onko solmu reunasolmu (1) vai sisäsolmu (0)
solmut = [[0.0 0.0 1]; [1.0 0.0 1]; [1.0 1.0 1]; [0.0 1.0 1]];   

% rivillä i on elementin numero i solmupistenumerot (koordinaatit löytyvät
% solmunumeron perusteella (sen ilmoittamalta riviltä) taulukosta solmut)
elementit = [[1 2 3 ]; [1 3 4 ]];