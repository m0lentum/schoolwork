function [solution, maxError] = w6_solver(pointCount, ...
    % 0 = Cholesky, 1 = Jacobi
    method)
  % sama määrä laskentapisteitä x- ja y-suunnissa
  rowPointCount = pointCount;
  columnPointCount = pointCount;
  stepLength = 1 / pointCount;
  stepLengthSq = stepLength^2;

  % diskreetti Laplace-operaattori lohkomatriisina
  blockT = diag(-4 * ones(rowPointCount, 1)) ...
         + diag(ones(rowPointCount - 1, 1), 1) ...
         + diag(ones(rowPointCount - 1, 1), -1);
  sideDiag = ones(rowPointCount * columnPointCount - rowPointCount, 1);
  laplaceMat = (blkdiag(repmat({blockT}, 1, columnPointCount){:}) ...
         + diag(sideDiag, rowPointCount) ...
         + diag(sideDiag, -rowPointCount)) ...
         / stepLengthSq;

  % lähdetermi ja tunnettu ratkaisu virheen tarkastelua varten
  f = @(x,y) 2 * pi^2 * sin(pi*x) + sin(pi*y);
  rhs = zeros(rowPointCount * columnPointCount, 1);

  refSolution = @(x,y) sin(pi*x)*sin(pi*y);
  refV = zeros(length(rhs), 1);

  x = 0 : stepLength : 1;
  y = 0 : stepLength : 1;
  for ix = 1 : rowPointCount
    for iy = 1 : columnPointCount
      rhs((iy-1) * rowPointCount + ix) = f(x(ix), y(iy));
      refV((iy-1) * rowPointCount + ix) = refSolution(x(ix), y(iy));
    end
  end

  % ratkaisu
  if method == 0
    solution = w6_cholesky(-laplaceMat, rhs);
  else
    solution = w6_jacobi(-laplaceMat, rhs);
  end
  maxError = max(abs(refV - solution));
end

