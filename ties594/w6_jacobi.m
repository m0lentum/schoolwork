function x = w6_jacobi(A, b)
  % ratkaisee yhtälön Ax = b käyttäen Jacobi-iteraatiota.

  Adiag = diag(A);
  Arest = A - Adiag;
  % diagonaalin käänteismatriisi käsin laskettuna
  AdiagInv = zeros(length(Adiag(1,:)), length(Adiag(:,1)));
  for id = 1 : length(Adiag(1,:))
    AdiagInv(id,id) = 1 / Adiag(id,id);
  end

  x = zeros(length(b), 1); % alkuarvaus x = 0
  while 1
    nextX = AdiagInv * (b - Arest * x);
    % konvergenssitarkistus:
    % jos ero edelliseen iteraatioon on riittävän pieni, lopeta
    if max(abs(nextX - x)) < 0.0001
      break;
    end
    x = nextX;
  end
end
