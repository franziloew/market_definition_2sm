% % Export Correlation Array to Excel
% 
for i=1:11;
    my_cell=sprintf('A%s',num2str(i));
    xlswrite('Correlation.xlsx',QS1(:,:,i),my_cell);
end

for i=1:11;
    my_cell=sprintf('A%s',num2str(i));
    xlswrite('Correlation.xlsx',QS2(:,:,i),my_cell);
end

for i=1:11;
    my_cell=sprintf('A%s',num2str(i));
    xlswrite('Correlation.xlsx',QQ(:,:,i),my_cell);
end

for i=1:11;
    my_cell=sprintf('A%s',num2str(i));
    xlswrite('Correlation.xlsx',SS(:,:,i),my_cell);
end

% Export to csv 

csvwrite('QQdiag.csv',QQdiag)