# FIFO MRI Re-analysis, R models

library(lme4)
library(lmerTest) # Automatically calculates p-values for lmer models
library(dplyr)

df <- read.csv("/Users/chrisiyer/_Current/lab/code/fan-color/fifo_mri/rdata_choice.csv")
df$sub_id <- as.factor(df$sub_id) # for random intercepts
df_fanin  <- df %>% filter(item_condition == "fanin_A")
df_fanout <- df %>% filter(item_condition == "fanout_B")

# ===========================================================================
# BLOCK 1: FAN-IN (A-Items)
# ===========================================================================
cat("\n--- FAN-IN (A) MODELS ---\n")

# Sim vs Accuracy
lmer_fanin_acc_sim <- lmer(correct ~ avg_pair_sim + (1 | sub_id), data = df_fanin)
summary(lmer_fanin_acc_sim)

# Rel Sim vs Accuracy
lmer_fanin_acc_rel <- lmer(correct ~ avg_relative_similarity + (1 | sub_id), data = df_fanin)
summary(lmer_fanin_acc_rel)

# Sim vs Log RT
lmer_fanin_rt_sim <- lmer(choice_rt_avg_log ~ avg_pair_sim + (1 | sub_id), data = df_fanin)
summary(lmer_fanin_rt_sim)

# Rel Sim vs Log RT
lmer_fanin_rt_rel <- lmer(choice_rt_avg_log ~ avg_relative_similarity + (1 | sub_id), data = df_fanin)
summary(lmer_fanin_rt_rel)


# ===========================================================================
# BLOCK 2: FAN-OUT (B-Items)
# ===========================================================================
cat("\n--- FAN-OUT (B) MODELS ---\n")

# Sim vs Accuracy
lmer_fanout_acc_sim <- lmer(correct ~ avg_pair_sim + (1 | sub_id), data = df_fanout)
summary(lmer_fanout_acc_sim)

# Rel Sim vs Accuracy
lmer_fanout_acc_rel <- lmer(correct ~ avg_relative_similarity + (1 | sub_id), data = df_fanout)
summary(lmer_fanout_acc_rel)

# Sim vs Log RT
lmer_fanout_rt_sim <- lmer(choice_rt_avg_log ~ avg_pair_sim + (1 | sub_id), data = df_fanout)
summary(lmer_fanout_rt_sim)

# Rel Sim vs Log RT
lmer_fanout_rt_rel <- lmer(choice_rt_avg_log ~ avg_relative_similarity + (1 | sub_id), data = df_fanout)
summary(lmer_fanout_rt_rel)



# ===========================================================================
# BLOCK 3: REACTIVATION (PROACTIVE AND REACTIVE)
# ===========================================================================
cat("\n--- REACTIVATION MODELS ---\n")

# Load and prepare Reward Phase Reactivation data
df_rew <- read.csv("/Users/chrisiyer/_Current/lab/code/fan-color/fifo_mri/rdata_reward.csv")
df_rew$sub_num <- as.factor(df_rew$sub_num)
df_rew_fanin <- df_rew %>% filter(condition == "fanin_A")
df_rew_fanout <- df_rew %>% filter(condition == "fanout_B")

# Load and prepare Choice Phase Reactivation data
df_choice <- read.csv("/Users/chrisiyer/_Current/lab/code/fan-color/fifo_mri/rdata_choice.csv")
df_choice$sub_id <- as.factor(df_choice$sub_id)
df_choice_fanin <- df_choice %>% filter(item_condition == "fanin_A")
df_choice_fanout <- df_choice %>% filter(item_condition == "fanout_A") # Transfer trials for fan-out logic test A decisions

# ---------------------------------------------------------------------------
# 1. FAN-IN A: PROACTIVE REACTIVATION (REWARD PHASE)
# ---------------------------------------------------------------------------
cat("\n--- 1. FAN-IN A: PROACTIVE (REWARD) ---\n")
lmer_rew_fanin_sim <- lmer(rew_reactivation_avg ~ pair_similarity + (1 | sub_num), data = df_rew_fanin)
summary(lmer_rew_fanin_sim)

lmer_rew_fanin_rel <- lmer(rew_reactivation_avg ~ pair_relative_similarity + (1 | sub_num), data = df_rew_fanin)
summary(lmer_rew_fanin_rel)

# ---------------------------------------------------------------------------
# 2. FAN-IN A: REACTIVE INFERENCE (CHOICE PHASE)
# ---------------------------------------------------------------------------
cat("\n--- 2. FAN-IN A: REACTIVE (CHOICE) ---\n")
lmer_choice_fanin_sim <- lmer(choice_react_avg ~ avg_pair_sim + (1 | sub_id), data = df_choice_fanin)
summary(lmer_choice_fanin_sim)

lmer_choice_fanin_rel <- lmer(choice_react_avg ~ avg_relative_similarity + (1 | sub_id), data = df_choice_fanin)
summary(lmer_choice_fanin_rel)

# ---------------------------------------------------------------------------
# 3. FAN-OUT B: PROACTIVE REACTIVATION (REWARD PHASE)
# ---------------------------------------------------------------------------
cat("\n--- 3. FAN-OUT B: PROACTIVE (REWARD) ---\n")
lmer_rew_fanout_sim <- lmer(rew_reactivation_avg ~ pair_similarity + (1 | sub_num), data = df_rew_fanout)
summary(lmer_rew_fanout_sim)

lmer_rew_fanout_rel <- lmer(rew_reactivation_avg ~ pair_relative_similarity + (1 | sub_num), data = df_rew_fanout)
summary(lmer_rew_fanout_rel)

# ---------------------------------------------------------------------------
# 4. FAN-OUT B: REACTIVE INFERENCE (CHOICE PHASE)
# ---------------------------------------------------------------------------
cat("\n--- 4. FAN-OUT B: REACTIVE (CHOICE) ---\n")
lmer_choice_fanout_sim <- lmer(choice_react_avg ~ avg_pair_sim + (1 | sub_id), data = df_choice_fanout)
summary(lmer_choice_fanout_sim)

lmer_choice_fanout_rel <- lmer(choice_react_avg ~ avg_relative_similarity + (1 | sub_id), data = df_choice_fanout)
summary(lmer_choice_fanout_rel)
