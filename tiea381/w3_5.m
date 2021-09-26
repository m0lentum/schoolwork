B = [1 1 1 0 0
     2 1 0 1 0
     1 0 0 0 1];
A = B.' * B;

% alkuarvaus (1, 1, ..., 1)
x = ones(length(A(:,1)), 1);
c = 0;
while true
  y = A*x;
  c = max(abs(y));
  x_next = (1 / c) * y;

  diff = norm(x_next - x);
  x = x_next;
  if diff <= 1e-6
    break;
  end
end

fprintf('%.6g\n', c);
% tarkistus vertaamalla varmasti toimivaan menetelmään
fprintf('%.6g\n', max(eig(A)));
