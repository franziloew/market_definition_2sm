%%% Initialize results-matrizes

RESULTS_c       = c';
% Save marginal costs.
RESULTS_p_c     = p_c';
RESULTS_p       = p_c';
% Save competitive and actual prices.
RESULTS_q_c     = q_c';
RESULTS_q       = q_c';
% Save competitive and actual quantities.
RESULTS_pi_c    = pi_c';
RESULTS_pi      = pi_c';
% Save competitive and actual profits.
RESULTS_HHI_q   = HHI_q;
RESULTS_HHI_rev = HHI_rev;
% Save HHI-values based on quantities and on revenues.
RESULTS_s_q     = s_q';
RESULTS_s_rev   = s_rev';
% Save market shares based on quantities and on revenues.
RESULTS_e_c     = e';
RESULTS_e       = e';
% Save competitive and actual demand elasticities.
RESULTS_marg_c  = marg';
RESULTS_marg    = marg';
% Save competitive and actual price-cost margins.
RESULTS_W_c     = W;
RESULTS_W       = W;
% Save competitive and actual welfare.
RESULTS_CS_c    = CS;
RESULTS_CS      = CS;
% Save competitive and actual consumer surplus.
RESULTS_mm      = mm';
% Save colluders.
RESULTS_coll    = (sum(mm)>0);
% Evaluate, if a period was competitive.
RESULTS_s       = s;
% Store the cost shock.