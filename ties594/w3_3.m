f = @(x,y) sin(x)*sin(2*y);
leftEdge = @(y) 0;
rightEdge = @(y) 0;
bottomEdge = @(x) 0;
topEdge = @(x) sin(x);

STEP_COUNT = 4;
INNER_STEP_COUNT = STEP_COUNT - 1;
STEP_LENGTH = pi / STEP_COUNT;
STEP_LENGTH_SQ = STEP_LENGTH^2;

% u:n kerroinmatriisi lohkomatriisina
blockT = diag(4*ones(INNER_STEP_COUNT, 1)) ...
       - diag(ones(INNER_STEP_COUNT - 1, 1), 1) ...
       - diag(ones(INNER_STEP_COUNT - 1, 1), -1);
sideDiag = -ones(INNER_STEP_COUNT^2 - 3, 1);
uCoefs = blkdiag(repmat({blockT}, 1, INNER_STEP_COUNT){:}) ...
       + diag(sideDiag, 3) ...
       + diag(sideDiag, -3);

% yhtalon oikea puoli
rhs = zeros(INNER_STEP_COUNT^2, 1);
% ei reunoja mukaan
x = STEP_LENGTH : STEP_LENGTH : pi - STEP_LENGTH;
y = STEP_LENGTH : STEP_LENGTH : pi - STEP_LENGTH;
for xi = 1 : INNER_STEP_COUNT
  for yi = 1 : INNER_STEP_COUNT
    rhsIdx = (yi-1) * INNER_STEP_COUNT + xi;
    rhs(rhsIdx) = STEP_LENGTH_SQ * f(x(xi), y(yi));
    % reuna-arvot
    if xi == 1
      rhs(rhsIdx) = rhs(rhsIdx) + leftEdge(y(yi));
    elseif xi == INNER_STEP_COUNT
      rhs(rhsIdx) = rhs(rhsIdx) + rightEdge(y(yi));
    end
    if yi == 1
      rhs(rhsIdx) = rhs(rhsIdx) + bottomEdge(x(xi));
    elseif yi == INNER_STEP_COUNT
      rhs(rhsIdx) = rhs(rhsIdx) + topEdge(x(xi));
    end
  end
end

solution = uCoefs \ rhs;

% piirto
% tarvitaan ratkaisu 2D-matriisina, lisataan myos mukaan reuna-arvot
solM = zeros(STEP_COUNT + 1, STEP_COUNT + 1);
for xi = 1 : INNER_STEP_COUNT
  for yi = 1 : INNER_STEP_COUNT
    solM(xi+1, yi+1) = solution((yi-1) * INNER_STEP_COUNT + xi);
  end
end
% reuna-arvot
x = 0 : STEP_LENGTH : pi;
y = 0 : STEP_LENGTH : pi;
solM(1, :) = leftEdge(y);
solM(STEP_COUNT + 1, :) = rightEdge(y);
solM(:, 1) = bottomEdge(x);
solM(:, STEP_COUNT + 1) = topEdge(x);

surf(x, y, solM)
xlabel('x')
ylabel('y')
zlabel('u')

