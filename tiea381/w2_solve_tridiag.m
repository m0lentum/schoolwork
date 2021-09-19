% diagonaalit vektoreina
function x = w2_solve_tridiag(main_diag, lower_diag, upper_diag, rhs)
  % tarkistetaan, että dimensiot oikein
  dim = length(main_diag);
  if length(lower_diag) != dim - 1 ...
      || length(upper_diag) != dim - 1 ...
      || length(rhs) != dim
    error("Dimensions of given vectors did not match");
  end

  % LU-hajotelma

  l_lower_diag = zeros(dim-1,1);
  u_upper_diag = zeros(dim-1,1);
  u_main_diag = zeros(dim,1);

  u_main_diag(1) = main_diag(1);
  u_upper_diag(1) = upper_diag(1);
  for i = 2 : dim-1
    % alemman diagonaalin indeksointi alkaa 1:stä rivillä 2!
    l_lower_diag(i-1) = lower_diag(i-1) / u_main_diag(i-1);
    u_main_diag(i) = main_diag(i) - u_upper_diag(i-1) * l_lower_diag(i-1);
    u_upper_diag(i) = upper_diag(i);
  end
  l_lower_diag(dim-1) = lower_diag(dim-1) / u_main_diag(dim-1);
  u_main_diag(dim) = main_diag(dim) - u_upper_diag(dim-1) * l_lower_diag(dim-1);

  % ratkaistaan yhtälö Ly = b etenevällä sijoituksella

  y = zeros(dim);
  y(1) = rhs(1);
  for i = 2 : dim
    y(i) = rhs(i) - y(i-1) * l_lower_diag(i-1);
  end

  % ratkaistaan yhtälö Ux = y takenevalla sijoituksella

  x = zeros(dim,1);
  x(dim) = y(dim) / u_main_diag(dim);
  for i = dim - 1 : -1 : 1
    x(i) = (y(i) - x(i+1) * u_upper_diag(i)) / u_main_diag(i);
  end
end
