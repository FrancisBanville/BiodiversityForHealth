## Description of the dataset

The dataset contains the names and characteristics of the indicators of the Kunming-Montreal Global Biodiversity Framework monitoring framework, as well as our evaluation of their connection to human, animal, plant, and environmental health and their usability for monitoring One Health action tracks. 

Variables are described in the following tables. 


### Names and characteristics of the indicators

| Variable | Description |
| -------- | ------- |
| `indicator_label` | Label of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf) (February 2025). |
| `indicator_name` | Name of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf) (February 2025). |
| `indicator_category` | Main category (headline, binary, component or complementary) of the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf) (February 2025). |
| `indicator_target` | Main goal or target monitored by the indicator, as provided in [CBD/COP/16/L.26/Rev.1](https://www.cbd.int/doc/c/1e13/f20d/81cd8447744640bbd21e008f/cop-16-l-26-rev1-en.pdf) (February 2025). |
| `indicator_GAP_category` | Category of the [Global Action Plan on Biodiversity and Health](https://www.cbd.int/health/GAP.shtml) (CBD/COP/DEC/16/19 - Annex III B) in which the target of the indicator falls. |


### Information about the evaluation

| Variable | Description |
| -------- | ------- |
| `evaluator_name` | Name of the evaluator who provided comments justifying the assessment |
| `evaluator_flag` | Flagged indicators highlight remaining uncertainties in the assessment |
| `evaluator_comments` | Justifications and comments provided by an evaluator |


### Evaluation of the link between KM-GBF indicators and health

| Variable | Description |
| -------- | ------- |
| `connection_human_health` | Connection of the indicator with human health (either directly connected, indirectly connected, potentially connected or not connected) |
| `connection_animal_health` | Connection of the indicator with animal health (either directly connected, indirectly connected, potentially connected or not connected) |
| `connection_plant_health` | Connection of the indicator with plant health (either directly connected, indirectly connected, potentially connected or not connected) |
| `connection_environmnental_health` | Connection of the indicator with environmental health (either directly connected, indirectly connected, potentially connected or not connected) |

### Evaluation of the usability of KM-GBF indicators for monitoring One Health action tracks

| Variable | Description |
| -------- | ------- |
| `AT1_health_systems_usability` | Usability of the indicator for monitoring the first Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT1_health_systems_action` | The main action that the indicator can monitor in the first action track, if any |
| `AT2_pandemic_zoonoses_usability` | Usability of the indicator for monitoring the second Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT2_pandemic_zoonoses_action` | The main action that the indicator can monitor in the second action track, if any |
| `AT3_endemic_zoonoses_usability` | Usability of the indicator for monitoring the third Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT3_endemic_zoonoses_action` | The main action that the indicator can monitor in the third action track, if any |
| `AT4_food_safety_usability` | Usability of the indicator for monitoring the fourth Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT4_food_safety_action` | The main action that the indicator can monitor in the fourth action track, if any |
| `AT5_antimicrobial_resistance_usability` | Usability of the indicator for monitoring the fifth Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT5_antimicrobial_resistance_action` | The main action that the indicator can monitor in the fifth action track, if any |
| `AT6_environment_usability` | Usability of the indicator for monitoring the sixth Action Track of the One Health Joint Plan of Action (either directly usable, usable after adaptation or not usable) |
| `AT6_environment_action` | The main action that the indicator can monitor in the sixth action track, if any |

