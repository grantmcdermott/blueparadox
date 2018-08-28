# The blue paradox: Preemptive overfishing in marine reserves

This repo contains the code and data for reproducing McDermott, Meng, *et al*. (PNAS, 2018): "[The blue paradox: Preemptive overfishing in marine reserves](http://dx.doi.org/10.1073/pnas.1802862115)". 

> **Abstract:** Most large-scale conservation policies are anticipated or announced in advance. This risks the possibility of preemptive resource extraction before the conservation intervention goes into force. We use a high-resolution dataset of satellite-based fishing activity to show that anticipation of an impending no-take marine reserve undermines the policy by triggering an unintended race-to-fish. We study one of the world’s largest marine reserves, the Phoenix Islands Protected Area (PIPA), and find that fishers more than doubled their fishing effort once this area was earmarked for eventual protected status. The additional fishing effort resulted in an impoverished starting point for PIPA equivalent to 1.5 y of banned fishing. Extrapolating this behavior globally, we estimate that if other marine reserve announcements were to trigger similar preemptive fishing, this could temporarily increase the share of overextracted fisheries from 65% to 72%. Our findings have implications for general conservation efforts as well as the methods that scientists use to monitor and evaluate policy efficacy.

Click on the "fork" button at the very top right of the page to create an independent copy of the repo within your own GitHub account. Alternately, click on the green "clone or download" button just below that to download the repo to your local computer.

## Software

The analysis for this project was conducted in both *R* and STATA. We have tried as far as possible to make the analysis fully reproducible using only the former, since it is an open-source programming language. You should be able to reproduce essentially all of the key results and figures using *R* on its own. However, some of the figures from the paper were produced using STATA and you will need to execute these (close-sourced) scripts to exactly match these cases. See the relevant README files in the `R` and `STATA` sub-directories for more information.

## Problems

If you have any trouble running the code, or find any errors, please file an issue and we'll look into it.

## License

The software code contained within this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php). The data and figures are made available under the [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/) license.