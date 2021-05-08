function x = w6_cholesky(A, b)
  % ratkaisee yhtälön Ax = b, jossa A on positiividefiniitti
  % symmetrinen matriisi, käyttäen Choleskyn hajotelmaa.

  % Cholesky-Banachiewicz-algoritmi
  L = zeros(length(A(1,:)), length(A(:,1)));
  for ir = 1 : length(A(1,:))
    for ic = 1 : ir
      s = 0;
      for ip = 1 : ic - 1
        s = s + L(ir,ip) * L(ic,ip);
      end
      if ir == ic
        L(ir,ic) = sqrt(A(ir,ic) - s);
      else
        L(ir,ic) = (1 / L(ic,ic)) * (A(ir,ic) - s);
      end
    end
  end

  % yhtälöryhmän ratkaisu yllä lasketun Cholesky-hajotelman avulla

  % ratkaistaan ensin Ly = b etenevällä sijoituksella
  % ("forward substitution", oma käännös)
  y = zeros(length(b), 1);
  for ir = 1 : length(b)
    s = 0;
    for ic = 1 : ir - 1
      s = s + L(ir,ic) * y(ic);
    end
    y(ir) = (b(ir) - s) / L(ir,ir);
  end
  % nyt L^T * x = y takenevalla sijoituksella
  LT = transpose(L);
  x = zeros(length(y), 1);
  for ir = length(y) : -1 : 1
    s = 0;
    for ic = ir + 1 : length(y)
      s = s + LT(ir,ic) * x(ic);
    end
    x(ir) = (y(ir) - s) / LT(ir,ir);
  end
end
