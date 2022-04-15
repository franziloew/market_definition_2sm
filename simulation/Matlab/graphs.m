% % % Graph of CrossCorrelation 3-D Matrix
% % 
figure;
surf(QQdiag(:,:))
xlabel('mu=theta');
ylabel('d+g');
zlabel('Corr.Coef.');
colorbar
matlab2tikz('QQ_d_g_mu.tikz', 'height', '\figureheight', 'width', '\figurewidth');
% 
figure;
surf(QSdiag1(:,:))
xlabel('mu=theta');
ylabel('d+g');
zlabel('Corr.Coef.');
colorbar
matlab2tikz('QS_d_g_mu.tikz', 'height', '\figureheight', 'width', '\figurewidth');
% % 
% % with mu=1
figure;
plot(QQdiag1(:,11))
xlabel('d+g');
ylabel('Corr. Coeff');
matlab2tikz('mu_1.tikz', 'height', '\figureheight', 'width', '\figurewidth');
% % 
% % with mu=.7
figure;
plot(QQdiag1(:,8))
xlabel('d+g');
ylabel('Corr. Coeff');
matlab2tikz('mu_7.tikz', 'height', '\figureheight', 'width', '\figurewidth');
% % 
% % with mu=.5
figure;
plot(QQdiag1(:,6))
xlabel('d+g');
ylabel('Corr. Coeff');
matlab2tikz('mu_5.tikz', 'height', '\figureheight', 'width', '\figurewidth');
% 
% % with mu=.3
figure;
plot(QQdiag1(:,4))
xlabel('d+g');
ylabel('Corr. Coeff');
matlab2tikz('mu_3.tikz', 'height', '\figureheight', 'width', '\figurewidth');

% % with mu=0
figure;
plot(QQdiag1(:,1))
xlabel('d+g');
ylabel('Corr. Coeff');
matlab2tikz('mu_0.tikz', 'height', '\figureheight', 'width', '\figurewidth');

% 
