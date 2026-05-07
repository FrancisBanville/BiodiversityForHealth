# Description of the dataset

The dataset `KMGBF_indicators_health.csv` contains the names and characteristics of the indicators of the Kunming-Montreal Global Biodiversity Framework (KM-GBF) monitoring framework, as well as our qualitative evaluation of their connection to human, animal, plant, and environmental health and their usability for monitoring One Health action tracks. Each indicator was independently evaluated by at least two evaluators, and a consensus was reached where there were initial disagreements. 
Variables are described in the following tables.

### Names and characteristics of the indicators

| Variable | Description |
| -------- | ------- |
| `indicator_label` | Label of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf). |
| `indicator_name` | Name of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf). |
| `indicator_category` | Main category (headline, binary, component or complementary) of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf).* |
| `indicator_target` | Main goal or target monitored by the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf). |
| `indicator_GAP_category` | Category of the [Global Action Plan on Biodiversity and Health](https://www.cbd.int/health/GAP.shtml) (CBD/COP/DEC/16/19 - Annex III B) in which the indicator falls.** |

*As set out in [CBD/COP/DEC/15/5](https://www.cbd.int/doc/decisions/cop-15/cop-15-dec-05-en.pdf):
- Headline indicators are a minimum set of high-level indicators that capture the overall scope of the goals and targets of the KM-GBF.
- Binary indicators are derived from binary (yes/no) responses to questions in national reports and are used for tracking global-level progress.
- Component indicators are a list of optional indicators that, together with the headline indicators, cover components of the goals and targets of the KM-GBF.
- Complementary indicators are a list of optional indicators for thematic or in-depth analysis of each goal and target.

**In the Global Action Plan on Biodiversity and Health (Annex III B), targets are grouped into 14 different categories. We assigned the category of an indicator based on the category of its target. Indicators that monitor goals A, B, C, and D of the KM-GBF are not assigned to any category.

## Evaluation of the link between KM-GBF indicators and health

| Variable | Description |
| -------- | ------- |
| `connection_human_health` | Strength of the link between the indicator and human health.† |
| `connection_animal_health` | Strength of the link between the indicator and animal health.† |
| `connection_plant_health` | Strength of the link between the indicator and plant health.† |
| `connection_environmnental_health` | Strength of the link between the indicator and environmental health.† |

† An indicator can be:
- Directly connected to health if it has a direct causal relationship with health (e.g., they directly measure the state or a risk factor of health).
- Indirectly connected to health if it is linked to health via a single intermediary factor.
- Potentially connected to health if it is linked to health via two or more intermediary factors, or if it is likely connected to health but we are not sure through which mechanism.
- Not connected to health if it has no connection with health, or if the connection is far-fetched, unlikely, or absent.

## Evaluation of the usability of KM-GBF indicators for monitoring One Health action tracks

| Variable | Description |
| -------- | ------- |
| `AT1_health_systems_usability` | Usability of the indicator for monitoring the first Action Track of the [One Health Joint Plan of Action](https://www.who.int/publications/i/item/9789240059139).‡ |
| `AT1_health_systems_action` | The main action that the indicator can monitor in the first action track, if any. |
| `AT2_pandemic_zoonoses_usability` | Usability of the indicator for monitoring the second Action Track of the One Health Joint Plan of Action.‡ |
| `AT2_pandemic_zoonoses_action` | The main action that the indicator can monitor in the second action track, if any. |
| `AT3_endemic_zoonoses_usability` | Usability of the indicator for monitoring the third Action Track of the One Health Joint Plan of Action.‡ |
| `AT3_endemic_zoonoses_action` | The main action that the indicator can monitor in the third action track, if any. |
| `AT4_food_safety_usability` | Usability of the indicator for monitoring the fourth Action Track of the One Health Joint Plan of Action.‡ |
| `AT4_food_safety_action` | The main action that the indicator can monitor in the fourth action track, if any. |
| `AT5_antimicrobial_resistance_usability` | Usability of the indicator for monitoring the fifth Action Track of the One Health Joint Plan of Action.‡ |
| `AT5_antimicrobial_resistance_action` | The main action that the indicator can monitor in the fifth action track, if any. |
| `AT6_environment_usability` | Usability of the indicator for monitoring the sixth Action Track of the One Health Joint Plan of Action.‡ |
| `AT6_environment_action` | The main action that the indicator can monitor in the sixth action track, if any. |

‡ An indicator can be: 
- Directly usable if it can already be used to monitor an action in the action track.
- Usable after adaptation if it needs to be slightly modified (e.g., changes in the scale of measurement, data resolution, or taxa identities) before being used to monitor an action in the action track.
- Not usable if it needs to be greatly modified before being used to monitor the actions in the action track, or if it monitors something outside the scope of the action track.

