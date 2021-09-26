A = [0 -49 0 1
     1  12 0 0
     0 -40 0 0
     0  78 1 0];
% tarkistusta varten
reference = eig(A);

while true
  [q,r] = qr(A);
  A = r * q;

  % A:n alakolmio-osan tarkistus
  if max(abs(tril(A,-1)(:))) < 1e-4
    break;
  end
end

% ominaisarvot diagonaalilla
diag(A)
reference
