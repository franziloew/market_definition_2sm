% Check correlation between and within Market 1&2

% pre-allocation to store the coefficients
QQ=zeros(19,19,11);
PP=zeros(19,19,11);

SS=zeros(19,19,11);
RR=zeros(19,19,11);

QS1=zeros(19,19,11);
QS2=zeros(19,19,11);
% 
% PR1=zeros(19,19,11);
% PR2=zeros(19,19,11);
%     

for d_counter1=1:19;
    
    for g_counter1=1:19;
        
        for mu_counter1=1:11;
            
            % ----Within-Market Correlation
            % --------Market 1----------------------------
            % ---------------- Quantity -------------------
            tempQQ=corrcoef(Q1(d_counter1,:,g_counter1,mu_counter1),Q2(d_counter1,:,g_counter1,mu_counter1));
            QQ(d_counter1,g_counter1,mu_counter1)=tempQQ(1,2);
            % ----------------- Price ---------------------
%             tempPP=corrcoef(P1(d_counter1,:,g_counter1,mu_counter1),P2(d_counter1,:,g_counter1,mu_counter1));
%             PP(d_counter1,g_counter1,mu_counter1)=tempPP(1,2);
%             
            % --------Market 2-----------------------------
            % ---------------- Quantity -------------------
            tempSS=corrcoef(S1(d_counter1,:,g_counter1,mu_counter1),S2(d_counter1,:,g_counter1,mu_counter1));
            SS(d_counter1,g_counter1,mu_counter1)=tempSS(1,2);
%             %           ------------------ Price --------------------
%             tempRR=corrcoef(R1(d_counter1,:,g_counter1,mu_counter1),R2(d_counter1,:,g_counter1,mu_counter1));
%             RR(d_counter1,g_counter1,mu_counter1)=tempRR(1,2);
            
            
            % ----Between-Market Correlation
            % --------Firm 1----------------------------
            % ---------------- Quantity -------------------
            tempQS1=corrcoef(Q1(d_counter1,:,g_counter1,mu_counter1),S1(d_counter1,:,g_counter1,mu_counter1));
            QS1(d_counter1,g_counter1,mu_counter1)=tempQS1(1,2);
            % ----------------- Price ---------------------
%             tempPR1=corrcoef(P1(d_counter1,:,g_counter1,mu_counter1),R1(d_counter1,:,g_counter1,mu_counter1));
%             PR1(d_counter1,g_counter1,mu_counter1)=tempPR1(1,2);
            
            % --------Firm 2-----------------------------
            % ---------------- Quantity -------------------
            tempQS2=corrcoef(Q2(d_counter1,:,g_counter1,mu_counter1),S2(d_counter1,:,g_counter1,mu_counter1));
            QS2(d_counter1,g_counter1,mu_counter1)=tempQS2(1,2);
%             % ----------------- Price ---------------------
%             tempPR2=corrcoef(P2(d_counter1,:,g_counter1,mu_counter1),R2(d_counter1,:,g_counter1,mu_counter1));
%             PR2(d_counter1,g_counter1,mu_counter1)=tempPR2(1,2);
             
        end
    end
end

% Get the Diagonals to analyse how the sum of d+g depends on mu, theta 

for i = 1:11; 
        
    tempQQ1=diag(QQ(:,:,i));
    QQdiag(:,i)=tempQQ1;
    
    tempSS1=diag(SS(:,:,i));
    SSdiag(:,i)=tempSS1;
    
    tempQS1diag=diag(QS1(:,:,i));
    QS1diag(:,i)=tempQS1diag;
    
    tempPP1=diag(PP(:,:,i));
    PPdiag(:,i)=tempPP1;
    
    tempPR1diag=diag(PR1(:,:,i));
    PR1diag(:,i)=tempPR1diag;
            
end
% 
% % Reshape to 2D Matrix
% QQdiag1 = reshape(QQdiag,[19,11]);
% QSdiag1 = reshape(QS1diag,[19,11]);
% PPdiag1 = reshape(PPdiag,[19,11]);
% PRdiag1 = reshape(PR1diag,[19,11]);



