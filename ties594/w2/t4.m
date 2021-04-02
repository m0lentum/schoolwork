STEP_COUNT = 50;
STEP_LENGTH = 1/STEP_COUNT;

% kertoimet toisen derivaatan approksimaatiolle
duCoefs = zeros(STEP_COUNT+1, STEP_COUNT+1);
duCoefs(1, 1) = STEP_LENGTH^2;
duCoefs(length(duCoefs(:,1)), length(duCoefs(1,:))) = STEP_LENGTH^2;
for i = 2 : length(duCoefs(:,1)) - 1
  duCoefs(i, i-1) = 1;
  duCoefs(i, i) = -2;
  duCoefs(i, i+1) = 1;
end
duCoefs = (1 / STEP_LENGTH^2) * duCoefs;

% lahdetermin f(x) = 1 - 2x^2 arvot
f = zeros(STEP_COUNT + 1, 1);
x = 0:STEP_LENGTH:1;
for i = 1 : STEP_COUNT
  f(i) = 1 - 2 * x(i)^2;
end
% reunaehdot toteutuvat implisiittisesti, koska f toteuttaa ne
% ja A:n diagonaalin paissa on 1 -> kertolasku ei muuta niita.

% ratkaisu
u = duCoefs \ f;

plot(x, u, '-o')
xlabel('x')
ylabel('u')
