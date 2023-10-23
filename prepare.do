/*
	All excel files from the temp help agency in the current working directory required
*/


////////////////// PRICE DATA /////////////////////

import excel "【提示用】【データ追加】開始票_201503-201602.xlsx",  firstrow clear
save  "kaishihyoadded-2015.dta", replace
import excel "【提示用】【データ追加】開始票_201603-201702.xlsx",  firstrow clear 
save  "kaishihyoadded-2016.dta", replace
import excel "【提示用】【データ追加】開始票_201703-201802.xlsx",  firstrow clear 
save  "kaishihyoadded-2017.dta", replace
import excel "【提示用】【データ追加】開始票_201803-201902.xlsx",  firstrow clear
save  "kaishihyoadded-2018.dta", replace
import excel "【提示用】【データ追加】開始票_201903-202002.xlsx",  firstrow clear 
save  "kaishihyoadded-2019.dta", replace
use kaishihyoadded-2015.dta, clear
forvalues i = 2016/2019{
	append using kaishihyoadded-`i'.dta
}
drop if 計上日==. // no missing values; this line drops the summary stats rows in the original excel
rename 計上日 date
rename 計上月 month
rename 見積受注取引先コード senttocode
rename 請求金額税抜乱数処理 billingamount_noised
rename 請求金額税抜百円単位 billingamount_rounded
rename 原価乱数処理 cost_noised
rename 原価百円単位 cost_rounded
rename 月額時間幅上限 hoursmax
rename 月額時間幅下限 hoursmin
rename 通常請求時間 hoursworked
destring senttocode, ignore("-") replace
// 1.Keep single record for each worker-month-client
sort ハッシュID month senttocode
bysort ハッシュID month senttocode: g N=_N
replace hoursworked = . if hoursworked > 1000 | hoursworked <= 1
foreach x in billingamount_noised cost_noised hoursmax hoursmin hoursworked{
	gen temp`x' = `x'
	replace temp`x' = . if N!=1
	sort ハッシュID month senttocode
	bysort senttocode: egen mean`x' = mean(temp`x')
	bysort ハッシュID month senttocode: egen avg`x' = mean(`x')
	bysort ハッシュID month senttocode: egen tot`x' = total(`x')
	replace `x' = cond(abs(avg`x' - mean`x') > abs(tot`x' - mean`x'), tot`x', avg`x') if N>1 
	replace `x' = tot`x' if avg`x' - mean`x'==. & tot`x' - mean`x'==. 
}
replace billingamount_noised=. if billingamount_noised<=0 | billingamount_rounded<=0
replace cost_noised=. if cost_noised<=0 | cost_rounded<=0
drop if missing(billingamount_noised) | missing(cost_noised) // 74488 obs
sort ハッシュID month senttocode
bysort ハッシュID month senttocode: g n=_n
drop if n!=1  // 72510 obs
drop n N mean* temp* tot* avg*

// 2. Impute hoursworked if 0
g hoursfrac = .
replace hoursfrac = (hoursworked - hoursmin - 通常超過時間 + 控除対象時間数 - 超過割増時間数 - 休日出勤時間数 - 深夜勤務時間数 - 休日深夜勤務時間数)/(hoursmax - hoursmin)
sort ハッシュID month senttocode
bysort ハッシュID senttocode: egen meanhoursfrac = mean(hoursfrac)
replace hoursworked = hoursmin + meanhoursfrac*(hoursmax - hoursmin) + 超過割増時間数 + 休日出勤時間数 + 深夜勤務時間数 + 休日深夜勤務時間数 if hoursworked == .
replace hoursworked = hoursmax + 通常超過時間 - 控除対象時間数 + 超過割増時間数 + 休日出勤時間数 + 深夜勤務時間数 + 休日深夜勤務時間数 if 通常超過時間 > 0 & hoursmax > 0 & !missing(通常超過時間) & !missing(hoursmax)
replace hoursworked = hoursmin + 通常超過時間 - 控除対象時間数 + 超過割増時間数 + 休日出勤時間数 + 深夜勤務時間数 + 休日深夜勤務時間数 if 控除対象時間数 > 0 & hoursmin > 0 & !missing(控除対象時間数) & !missing(hoursmin)
drop if hoursworked < 1 | hoursworked>480 // 71743 obs
drop meanhoursfrac
save "price.dta", replace // 71743 obs


////////////////// ENTRY DATA /////////////////////
import excel "【提示用】入社年月.xlsx",  firstrow clear
rename 入社年 entryyr
rename 入社月 entrymo
destring entryyr entrymo, ignore("-") replace
g entrydate = (entryyr - 1960)*12 + entrymo - 1
format entrydate %tm
drop if missing(entrydate)
save "entry.dta", replace




////////////////// APTITUDE DATA /////////////////////
import excel "【提示用】【データ追加】適性検査.xlsx", sheet("適正検査") firstrow clear
rename 言語正答率 言語2正答率 
rename T 言語6正答率 
rename 言語得点 言語2得点
rename U 言語6得点
rename 言語偏差値 言語2偏差値
rename V 言語6偏差値
drop O // in the orginal sheet Column O takes the same value as Column N if not missing
destring 自己リーダー-自己独創斬新, replace
save "aptitudesheet1.dta", replace
import excel "【提示用】【データ追加】適性検査.xlsx", sheet("追加1") firstrow clear
rename 言語正答率 言語2正答率 
rename T 言語6正答率 
rename 言語得点 言語2得点
rename U 言語6得点
rename 言語偏差値 言語2偏差値
rename V 言語6偏差値
drop O // in the orginal sheet Column O takes the same value as Column N if not missing
destring 一般的, force replace
drop if missing(一般的)
save "aptitudesheet2.dta", replace
import excel "【提示用】【データ追加】適性検査.xlsx", sheet("追加2") firstrow clear
rename 言語正答率 言語6正答率 
rename 言語得点 言語6得点
rename 言語偏差値 言語6偏差値
drop O // in the orginal sheet Column O takes the same value as Column N if not missing
save "aptitudesheet3.dta", replace

use aptitudesheet1, clear // 2919
append using aptitudesheet2, force // 4523
append using aptitudesheet3 // 4569
// 1. Destring and recode variables
g stress_tolerance =.
replace stress_tolerance=1 if  ストレス耐性=="とても弱い"
replace stress_tolerance=2 if  ストレス耐性=="弱い" 
replace stress_tolerance=3 if  ストレス耐性=="やや弱い" 
replace stress_tolerance=4 if  ストレス耐性=="普通"
replace stress_tolerance=5 if  ストレス耐性=="強い"
label variable stress_tolerance ストレス耐性
destring ストレス震度, generate(stress_magnitude) ignore("◎" "震度")
replace stress_magnitude=0 if ストレス震度=="◎" 
replace stress_magnitude=0 if stress_magnitude==8
label define grade 1 "E" 2 "D" 3 "C" 4 "B" 5 "A"
g overall_assessment = .
replace overall_assessment = 1 if 総合判定 == "E"
replace overall_assessment = 2 if 総合判定 == "D"
replace overall_assessment = 3 if 総合判定 == "C"
replace overall_assessment = 4 if 総合判定 == "B"
replace overall_assessment = 5 if 総合判定 == "A"
g aptitude_assessment = .
replace aptitude_assessment = 1 if 判定結果 == "E"
replace aptitude_assessment = 2 if 判定結果 == "D"
replace aptitude_assessment = 3 if 判定結果 == "C"
replace aptitude_assessment = 4 if 判定結果 == "B"
replace aptitude_assessment = 5 if 判定結果 == "A"
label values overall_assessment aptitude_assessment grade
label variable overall_assessment 総合判定
label variable aptitude_assessment 判定結果
drop ストレス震度 ストレス耐性 総合判定 判定結果
drop レコードタイプ // take the string "適性検査" for all
drop 職務 // no obs
drop 組織型 // no obs
drop CI CJ CK
// 2. Rename and label variables
rename 一般的 type_general
rename 活動型気分性 activity_mood
rename 活動型身体性 activity_physical
rename 感情安定性 mental_steadiness 
rename 危機耐性 crisis_tolerance
rename 客観科学型 interest_science
rename 求知欲求 desire_for_knowledge
rename 共感性 empathy 
rename 協調性 cooperativeness
rename 勤労意欲 willness_to_work
rename 顕示欲求 desire_for_selfexposure
rename 言語2正答率 verbal2_percentagecorrect 
rename 言語2得点 verbal2_score
rename 言語2偏差値 verbal2_tscore
rename 言語6正答率 verbal6_percentagecorrect 
rename 言語6得点 verbal6_score
rename 言語6偏差値 verbal6_tscore
rename 思索型客観性 thinking_objectiveness
rename 思索型内閉性 thinking_meditation
rename 指導性 leadership 
rename 支配欲求 desire_for_domination
rename 自己信頼性 confidence
rename 自己リーダー self_leadership1
rename 自己意欲熱意 self_enthusiasm
rename 自己企画立案 self_planning
rename 自己決断勇気 self_resolution
rename 自己現状分析 self_analysis
rename 自己根気強さ self_perseverance
rename 自己指導力 self_leadership2
rename 自己自己信頼 self_confidence
rename 自己情報活用 self_information
rename 自己積極実行 self_activeness
rename 自己責任感 self_responsibility 
rename 自己折衝力 self_negotiation_ability
rename 自己専門知識 self_expert_knowledge
rename 自己調整力 self_adjustment
rename 自己洞察力 self_insight
rename 自己独創斬新 self_innovation 
rename 自主性 independence
rename 自制型弱気さ selfcontrol_timidity
rename 自制型慎重性 selfcontrol_prudence
rename 自律欲求 desire_for_autonomy
rename 社会経済型 interest_socioeconomic
rename 集中力 type_concentrated
rename 従順性 obedience
rename 信頼係数 reliability
rename 審美芸術型 interest_art
rename 心理情緒型 interest_emotion
rename 親和欲求 desire_for_affiliation
rename 図形正答率 graphics_percentagecorrect 
rename  図形得点 graphics_score 
rename 図形偏差値 graphics_tscore 
rename 数理正答率 math_percentagecorrect 
rename 数理得点 math_score 
rename 数理偏差値 math_tscore 
rename 精神力 type_resilient
rename 積極型競争性 positive_competitiveness
rename 積極型自尊心 positive_selfrespect
rename 積極性 positive_attitude
rename 責任感 sense_of_responsibility 
rename 素直さ honesty 
rename 総合正答率 overall_percentagecorrect 
rename 総合得点 overall_score 
rename 総合偏差値 overall_tscore 
rename 足腰 type_physical
rename 達成欲求 desire_for_achievement
rename 秩序欲求 desire_for_rule
rename 定着安定 stableness
rename 努力型規則性 effort_regularity
rename 努力型持続性 effort_persistence
rename 日常周辺事型 interest_dailylife
rename 標準化 type_standardized
rename 物質的欲望 material_desire
rename 論理正答率 logic_percentagecorrect 
rename 論理得点 logic_score
rename 論理偏差値 logic_tscore
rename ﾓﾗﾄﾘｱﾑ傾向 moratorium_tendency
// 3. Drop ids with missing values in main variables, imputing missing values with sample mean for other variables
drop if missing(stress_tolerance) | missing(overall_tscore) | missing(aptitude_assessment) | missing(overall_assessment) // 4301 obs

sort ハッシュID aptitude_assessment
bysort ハッシュID: g n = _n
drop if n > 1 // 4211 obs
drop n
local imputelist verbal2_percentagecorrect  verbal2_score  verbal2_tscore  graphics_percentagecorrect  graphics_score  graphics_tscore  honesty  type_physical
foreach name in `imputelist'{
	egen t`name'=mean(`name')
	replace `name'=t`name' if missing(`name')
	drop t`name'
}
save "aptitude.dta", replace // 4211





////////////////// INFO DATA /////////////////////
// 1. Merge 3 basic information data sets
import excel "【提示用】社員情報.xlsx",  firstrow clear
save "basicinfo.dta", replace
import excel "【提示用】出身地.xlsx",  firstrow clear
save "birthplace.dta", replace
import excel "【提示用】生年年.xlsx",  firstrow clear
save "birthyear.dta", replace
use basicinfo.dta, clear
local filenames birthplace birthyear
foreach f of local filenames{
	merge m:1 ハッシュID using `f'.dta, nogenerate
}
// 2. Destring variables 
destring 退職済 偏差値 生年, replace force 
destring 等級, ignore("A" "B" "・") generate(level_rounded)
encode ステイタス, generate(status)
drop ステイタス
encode 拠点, generate(branch)
drop 拠点
encode 雇用契約種類, generate(contract)
drop 雇用契約種類
encode 最終学歴, generate(education)
replace education=. if education==12 // 未入力
drop 最終学歴
recode education (2/4 16 17 = 1) (11 13/15 = 2) (5/7 = 3) (8/10 = 4) (1 = 5), generate(education_cate) // 1: 高卒/高専/大学中退; 2: 専門卒/短大; 3: 大学卒; 4: 大学院卒; 5: その他
label define education_cate 1 "high school graduate / university dropout" 2 "junior college graduate" 3 "university graduate" 4 "graduate school graduate" 5 "others"
label value education_cate education_cate
recode education_cate (1=12) (2=14) (3=16) (4=18) (5=.), generate(education_year)
encode 国立ｏｒ私立ｏｒその他, generate(schooltype)
drop 国立ｏｒ私立ｏｒその他
encode 会社員ｏｒ公務員ｏｒその他, generate(publicworker)
drop 会社員ｏｒ公務員ｏｒその他
encode 所属本部, generate(headquarter)
drop 所属本部
encode 性別, generate(gender)
drop 性別
replace gender = gender - 1
label define gender 1 "Male" 0 "Female", replace
label value gender gender
encode 直近の待機理由, generate(reason_recent_waiting)
drop 直近の待機理由
encode 役職, generate(position)
recode position (1 = .) (4 = 1) (6 = 2) (5 = 3) (2 = 4) (3 = 5) (7 = 6) (8 = 7), generate(position_code)
recode position_code (1 2 = 2) (6 7 = 6), generate(position_cate) 
drop 役職
replace 出身地="茨城県" if 出身地=="茨城市"
encode 出身地, generate(origin)
replace origin=. if origin==26 // 未入力
drop 出身地
// 3. Rename variables
rename 入社日半期ごと entryrange
rename 退職日 resignation_date 
rename 退職済 retired 
rename 偏差値 tscore 
rename 受託実原価1時間あたり乱数処理 actual_hourly_cost_noised
rename 受託実原価1時間あたり百円単位 actual_hourly_cost_rounded
replace actual_hourly_cost_rounded=. if actual_hourly_cost_rounded<=0 // replace observations with 0 and negative values in rounded prices to missing
replace actual_hourly_cost_noised=. if actual_hourly_cost_rounded==.
rename 直接原価乱数処理 direct_cost_noised
rename 直接原価百円単位 direct_cost_rounded
replace direct_cost_rounded=. if direct_cost_rounded<=0
replace direct_cost_noised=. if direct_cost_rounded==.
rename 等級 level
rename 生年 birthyear  //4522 obs
// 4. Drop ids with missing values in main variables, imputing missing values with sample mean for tscore (偏差値)
drop if missing(birthyear) // 4158 obs
drop if missing(education_year) // 3634 obs
drop if missing(actual_hourly_cost_rounded) | missing(direct_cost_rounded) // 3195 obs
egen ttscore = mean(tscore), by(education)
replace tscore=ttscore if missing(tscore)
drop ttscore // tscore (偏差値) is only observed for undergraduate

g temp = daily(resignation_date, "MDY")
format temp %td
g resignation_month = mofd(temp)
format resignation_month %tm
drop temp
order resignation_month, after(resignation_date)
save "info.dta", replace // 3195 obs, missing in tscore, level, branch, contract, schooltype, reason_recent_waiting, and origin
