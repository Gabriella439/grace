module Grace.Infer where

import Grace.Context (Context)
import Grace.Input (Input)
import Grace.Location (Location)

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
      --   original paper)

    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)

    , input :: Input
      -- ^ The parent import, used to resolve relative imports
    }
