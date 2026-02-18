# govhrcast: Wage Bill Simulation Engine – Prompt File

## Purpose of the Package

The goal of `govhrcast` is to provide World Bank country teams a **simulation system** for public sector analytics. The package allows users to:

- Simulate workforce policies
- Estimate the wage bill under baseline or specific policy scenarios
- Plug-and-play policy rules and modules for retirement, hiring, promotion, and transfers
- Using the harmonized data format as developed in the govhr package i.e. `govhr::harmonization_dict()`
- Prepare the system for eventual stochastic extensions and uncertainty quantification as well as other future innovations like agent based modelling etc. 

---

## Conceptual Model

- **Government at time t** has been in existence for `N` years (N ≥ 1)
- **Contracts (C)** are assigned to **Personnel (P)** (C ≥ P)
- Simulations operate on **contract\_dt** and **personnel\_dt** modules (exact format of data as in govhr::bra_hrmis_contract |> as.data.table(); govhr::bra_hrmis_personnel |> as.data.table())
- Stock-flow identity must hold at each step:

```
Active_t =
  Active_{t-1}
  - Retirements_t
  - Other_Exits_t
  + Hires_t
```

---

## Simulation Blueprint

### General Procedure

1. Start at reference time `t = 0`, compute initial wage bill statistics
2. Increment time by the simulation interval (year, month, or day)
3. Apply **Retirement Module**
4. Apply **Hiring Module**
5. Apply **Promotion Module**
6. Apply **Transfer Module**
7. Repeat steps 2–6 for each simulation period

Each of the modules in 3-6 can also be standalone in case the user wants only to simulate, for instance, retirement, hiring, promotion or transfers

---
