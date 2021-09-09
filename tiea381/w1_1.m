unstable = @(a, b, c) (-b + sqrt(b^2 - 4*a*c)) / (2*a);
stable = @(a, b, c) (-2*c) / (b + sqrt(b^2 - 4*a*c));

% tehtävä: a ~= 1, b > 0, c ~= 0
% asetetaan a ja c aluksi 1 ja 0:ksi ja b:ksi joku vakio > 0,
% ja lisätään virhettä asteittain
b = 15;
for i = 1 : 5
    % virhe, aluksi 1/10_000, lopuksi 1/10
    err = 0.000001 * 10^i
    a = 1.0 - err;
    c = 0.0 + err;

    a_s = cast(a, 'single');
    b_s = cast(b, 'single');
    c_s = cast(c, 'single');

    unst_d = unstable(a, b, c);
    unst_s = unstable(a_s, b_s, c_s);
    st_d = stable(a, b, c);
    st_s = stable(a_s, b_s, c_s);
    fprintf('unstable, double: %.6e, single: %.6e, difference: %.6e\n',...
        unst_d, unst_s, abs(unst_d - unst_s));
    fprintf('stable, double: %.6e, single: %.6e, difference: %.6e\n',...
        st_d, st_s, abs(st_d - st_s));
end
