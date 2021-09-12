f = @(phi) phi - sin(phi) - 0.2;

% karkea alkuarvaus - ratkaisu varmasti
% ainakin nollan ja piin välissä
phi_prev = 0;
phi = 2;

while true
  phi_next = phi - (f(phi) * (phi - phi_prev)) / (f(phi) - f(phi_prev));
  phi_prev = phi;
  phi = phi_next;
  if abs(phi - phi_prev) < 1e-3
    break;
  end
end

fprintf('%.3g\n', phi);
