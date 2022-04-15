clear; close all; clc;

n=10000; % Setting the number of time periods equal to 10000.
b=1;
rho=.3; %This is the coefficient on the lagged part of x

x=zeros(n,1); % Initialise the vector x
x(1)=0;

for i = 2:n 
    x(i)=rho*x(i-1)+b*randn();
end

zoom=1.0;
FigHandle = figure('Position', [750, 300, 1049*zoom, 895*zoom]);
plot(x, 'LineWidth', 1.4)
ylabel('X(t)')
xlabel('t')