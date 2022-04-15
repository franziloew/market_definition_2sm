clear;
clc;

% Two-sided-Market Oligopoly Model with 2 Symmetric Firms. 
% Each Firms faces two inverse Demand Functions 
% ----------------------------Firm 1--------------------------------------
syms q1 s1 q2 s2 d g theta mu c1 c2 k1 k2
p1=1-q1-mu*q2+d*s1;      %Market 1
r1=1-s1-theta*s2+g*q1;         %Market 2
% ----------------------------Firm 2--------------------------------------
p2=1-q2-mu*q1+d*s2;      %Market 1
r2=1-s2-theta*s1+g*q2;         %Market 2

% Cost Function: Both firms face a common and an individual cost-shock on
% both Markets. 
% K(q1,s1) = c1 + k1 + F , with F=0
% K(q2,s2) = c2 + k2 + F , with F=0

% Profit Functions
% ------------------------------Firm1-------------------------------------
pi1=(p1-c1)*q1+(r1-k1)*s1; 
% ------------------------------Firm2-------------------------------------
pi2=(p2-c2)*q2+(r2-k2)*s2;

% Take Derivatives 
eqn1=diff(pi1,q1);
eqn2=diff(pi1,s1);
eqn3=diff(pi2,q2);
eqn4=diff(pi2,s2);

% and solve for q1,s1,q2,s2
sol = solve([eqn1, eqn2, eqn3, eqn4], [q1,s1,q2,s2]);

% Convert symbolic expression to function handle for q1,q2,s1,s2
q1 = matlabFunction(sol.q1);
s1 = matlabFunction(sol.s1);
q2 = matlabFunction(sol.q2);
s2 = matlabFunction(sol.s2);
p1 = matlabFunction(subs(p1));
p2 = matlabFunction(subs(p2));
r1 = matlabFunction(subs(r1));
r2 = matlabFunction(subs(r2));
 

Q1=zeros(19,1000,19,11);
Q2=zeros(19,1000,19,11);
S1=zeros(19,1000,19,11);
S2=zeros(19,1000,19,11);
% 
P1=zeros(19,1000,19,11);
P2=zeros(19,1000,19,11);
R1=zeros(19,1000,19,11);
R2=zeros(19,1000,19,11);
% % 
% Q1=cell(11,1);
% Q2=cell(11,1);
% S1=cell(11,1);
% S2=cell(11,1);

loop_counter=1;

mu_counter = 1;

for mu = 0:.1:1
    
    d_counter = 1;
    
    for d = -.45:.05:.45
        
        g_counter = 1;
        
        for g =-.45:.05:.45
            
              theta=mu;
              
                % Modelling Marginal Costs
                % ----------------Market 1-------------------------
                % Symmetric Shock on Market 1
                shock_c1 = randi([1 10],1,10)/1000;  %creates a 1x100 
                % Vector with uniformly distrib. random numbers between 0 and 0.01
                % Asymmetric Shock on Market 1
                shock_c11 = randi([1 10],1,10)/1000; % Firm1
                shock_c21 = randi([1 10],1,10)/1000; % Firm2

                c1 = shock_c1+shock_c11; %MC Firm1/Market1
                c2 = shock_c1+shock_c21; %MC Firm2/Market1
                % ----------------Market 2-------------------------
                % Symmetric Shock on Market 2
                shock_c2 = randi([1 10],1,10)/1000;
                % Asymmetric Shock on Market 1
                shock_c12 = randi([1 10],1,10)/1000; %Firm 1
                shock_c22 = randi([1 10],1,10)/1000; %Firm 2

                k1 = shock_c2+shock_c12; %MC Firm1/Market2
                k2 = shock_c2+shock_c22; %MC Firm2/Market2

                % Substitute Symbols in functions 
                try          
                    % With cell arrays

%                 Q1{mu_counter,1}(d_counter,:,g_counter) = double(subs(q1));
%                 Q2{mu_counter,1}(d_counter,:,g_counter) = double(subs(q2));
%                 S1{mu_counter,1}(d_counter,:,g_counter) = double(subs(s1));
%                 S2{mu_counter,1}(d_counter,:,g_counter) = double(subs(s2));
%                 P1{mu_counter,1}(d_counter,:,g_counter) = double(subs(p1));
%                 P2{mu_counter,1}(d_counter,:,g_counter) = double(subs(p2));
%                 R1{mu_counter,1}(d_counter,:,g_counter) = double(subs(r1));
%                 R2{mu_counter,1}(d_counter,:,g_counter) = double(subs(r2));
% 
                     % With 4d Matrices
                     Q1(g_counter,:,d_counter,mu_counter) = double(subs(q1));
                     Q2(g_counter,:,d_counter,mu_counter) = double(subs(q2));
                     S1(g_counter,:,d_counter,mu_counter) = double(subs(s1));
                     S2(g_counter,:,d_counter,mu_counter) = double(subs(s2));
                     P1(g_counter,:,d_counter,mu_counter) = double(subs(p1));
                     P2(g_counter,:,d_counter,mu_counter) = double(subs(p2));
                     R1(g_counter,:,d_counter,mu_counter) = double(subs(r1));
                     R2(g_counter,:,d_counter,mu_counter) = double(subs(r2));
                catch
                    disp('Error Division by Zero')
                end

                
         
                g_counter=g_counter+1;
                loop_counter=loop_counter+1
                
                
        end
        
        d_counter=d_counter+1;
        
    end
    
    mu_counter=mu_counter+1;
    
end

    

