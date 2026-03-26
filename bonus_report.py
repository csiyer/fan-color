import os, glob
import pandas as pd

DATA_DIR = 'data'
OUTPUT_FILE = 'prolific_bonuses.csv'

def generate_bonus_report():
    files = glob.glob(os.path.join(DATA_DIR, '*.csv'))
    if not files:
        print(f"No CSV files found in {DATA_DIR}")
        return

    bonuses = []
    for f in files:
        try:
            df = pd.read_csv(f)
            # Find the final summary trial
            summary = df[df['phase'] == 'final_summary']
            
            if not summary.empty:
                # Get ID and Bonus
                pid = summary['prolific_id'].iloc[0]
                bonus = summary['final_bonus_earned'].iloc[0]
                
                # If Prolific ID is missing/unknown, use the internal subject_id for now
                if pd.isna(pid) or pid == 'unknown' or str(pid).strip() == '':
                    pid = summary['subject_id'].iloc[0]
                
                bonuses.append({
                    'participant_id': pid,
                    'amount': bonus
                })
        except Exception as e:
            print(f"Error processing {os.path.basename(f)}: {e}")

    if bonuses:
        df_bonus = pd.DataFrame(bonuses)
        # Drop duplicates in case participant has multiple files (keep last summary)
        df_bonus = df_bonus.drop_duplicates(subset=['participant_id'], keep='last')
        
        df_bonus.to_csv(OUTPUT_FILE, index=False)
        print(f"\nSuccessfully generated {OUTPUT_FILE}")
        print(f"Total participants to bonus: {len(df_bonus)}")
        print("\nPreview:")
        print(df_bonus.to_string(index=False))
    else:
        print("No 'final_summary' rows found in the data files.")

if __name__ == "__main__":
    generate_bonus_report()
