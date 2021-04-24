% laskenta-alue [0, 1), hilaväli 0.25
STEP_COUNT = 4;
STEP_LENGTH = 1 / STEP_COUNT;

% u:n kerroinmatriisi lohkomatriisina.
% tässä oli viimeksi virhe, jonka takia -1-diagonaalit
% tulivat vääriin kohtiin kun N != 4.
blockT = diag(4*ones(STEP_COUNT, 1)) ...
       - diag(ones(STEP_COUNT - 1, 1), 1) ...
       - diag(ones(STEP_COUNT - 1, 1), -1);
sideDiag = -ones(STEP_COUNT^2 - STEP_COUNT, 1);
uCoefs = blkdiag(repmat({blockT}, 1, STEP_COUNT){:}) ...
       + diag(sideDiag, STEP_COUNT) ...
       + diag(sideDiag, -STEP_COUNT);

% yhtälön oikea puoli on tällä kertaa pelkkiä nollia
% lukuunottamatta yläreunaa y = 1 vasten olevia alkioita,
% joihin tulee reunaehto 1
rhs = zeros(STEP_COUNT^2, 1);
x = 0 : STEP_LENGTH : 1 - STEP_LENGTH;
y = 0 : STEP_LENGTH : 1 - STEP_LENGTH;
% yläreunan ehto
for xi = 1 : STEP_COUNT
  yi = STEP_COUNT;
  rhsIdx = (yi-1) * STEP_COUNT + xi;
  rhs(rhsIdx) = rhs(rhsIdx) + 1;
end

solution = uCoefs \ rhs;

% piirto
% tarvitaan ratkaisu 2D-matriisina, lisätään myös mukaan
% ylä- ja oikean reunan Dirichlet-ehdot
solM = zeros(STEP_COUNT + 1, STEP_COUNT + 1);
for xi = 1 : STEP_COUNT
  for yi = 1 : STEP_COUNT
    solM(xi+1, yi+1) = solution((yi-1) * STEP_COUNT + xi);
  end
end
% reuna-arvot yläreunaan
solM(:, STEP_COUNT + 1) = 1;

x = 0 : STEP_LENGTH : 1;
y = 0 : STEP_LENGTH : 1;
surf(x, y, solM)
xlabel('x')
ylabel('y')
zlabel('u')

