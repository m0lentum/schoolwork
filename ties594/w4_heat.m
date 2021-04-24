function result = w4_heat(...
  source, initState, diffusivity, ...
  % tehtävää 3 vastaavasti Dirichlet-ehdot pystysuunnassa
  % ja Neumann-ehdot du/dn=0 vaakasuunnassa, niin
  % sama ratkaisija toimii tehtäviin 2 ja 3
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  % vipu aika-askellusmenetelmän valintaa varten,
  % 0 = eksplisiittinen Euler, 1 = implisiittinen Euler
  timeStepMethod)

  % laskenta-alue [0, 1] x (0, 1)
  spatialStepLength = 1 / spatialStepCount;
  stepLengthSq = spatialStepLength^2;
  % x-suunnassa reunapisteet mukana laskennassa Neumann-reunaehtoja varten
  rowPointCount = spatialStepCount + 1;
  columnPointCount = spatialStepCount - 1;

  % diskreetti Laplace-operaattori lohkomatriisina
  % laskenta-alue ei nyt ole neliön muotoinen,
  % joten dimensiot muuttuvat hieman
  blockT = diag(-4 * ones(rowPointCount, 1)) ...
         + diag(ones(rowPointCount - 1, 1), 1) ...
         + diag(ones(rowPointCount - 1, 1), -1);
  sideDiag = ones(rowPointCount * columnPointCount - rowPointCount, 1);
  laplaceMat = (blkdiag(repmat({blockT}, 1, columnPointCount){:}) ...
         + diag(sideDiag, rowPointCount) ...
         + diag(sideDiag, -rowPointCount)) ...
         / stepLengthSq;
  % termi yhtälössä on -alpha * laplace u
  uCoefMat = -diffusivity * laplaceMat;

  % yhtälön oikea puoli (lähteet + reunaehdot)
  rhs = zeros(rowPointCount * columnPointCount, 1);
  % reunat mukaan vain x-suunnassa
  x = 0 : spatialStepLength : 1;
  y = spatialStepLength : spatialStepLength : 1 - spatialStepLength;
  for xi = 1 : rowPointCount
    for yi = 1 : columnPointCount
      rhsIdx = (yi-1) * rowPointCount + xi;
      rhs(rhsIdx) = source(x(xi), y(yi));
      % reuna-arvot
      if yi == 1
        rhs(rhsIdx) = rhs(rhsIdx) + bottomDirichlet(x(xi)) ...
                    * diffusivity / stepLengthSq;
      elseif yi == columnPointCount
        rhs(rhsIdx) = rhs(rhsIdx) + topDirichlet(x(xi)) ...
                    * diffusivity / stepLengthSq;
      end
    end
  end

  % aikadiskretointi ja -askellus
  dt = endTime / timeStepCount;
  t = 0 : dt : endTime;

  % lasketaan aluksi vektoreina, mutta palautetaan lopuksi matriiseina
  % piirtämistä varten
  resultVecs = zeros(length(rhs), timeStepCount);
  % alkutila annetusta alkuehdosta
  for xi = 1 : rowPointCount
    for yi = 1 : columnPointCount
      resultVecs((yi-1) * rowPointCount + xi,1) = initState(x(xi), y(yi));
    end
  end

  if timeStepMethod == 0
    % eksplisiittinen Euler
    for ti = 2 : timeStepCount
      prevU = resultVecs(:,ti-1);
      resultVecs(:,ti) = prevU + dt * (-uCoefMat * prevU + rhs);
    end
  else
    % implisiittinen Euler
    for ti = 2 : timeStepCount
      prevU = resultVecs(:,ti-1);
      resultVecs(:,ti) = (eye(length(uCoefMat)) + dt * uCoefMat) ...
        \ (prevU + dt * rhs);
    end
  end

  % muokataan ratkaisu 3D-matriisiksi jossa x-, y- ja t-ulottuvuudet
  % ja reunapisteet mukana myös y-suunnassa
  result = zeros(spatialStepCount + 1, spatialStepCount + 1, timeStepCount);
  for ti = 1 : timeStepCount
    for xi = 1 : rowPointCount
      for yi = 1 : columnPointCount
        % yi+1 koska y-suunnassa reunapisteet eivät ole laskenta-alueessa
        result(xi, yi+1, ti) = resultVecs((yi-1) * rowPointCount + xi, ti);
      end
    end
    % dirichlet-reunaehdot
    result(:, 1, ti) = bottomDirichlet(x);
    result(:, spatialStepCount + 1, ti) = topDirichlet(x);
  end
end

