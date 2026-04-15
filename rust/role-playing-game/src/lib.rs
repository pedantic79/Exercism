pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        Some(Self {
            health: 100,
            mana: Some(100),
            level: self.level,
        })
        .filter(|_| self.health == 0)
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(amount) if amount >= mana_cost => {
                self.mana = Some(amount - mana_cost);
                mana_cost * 2
            }
            Some(_) => 0,
            None => {
                self.health = self.health.saturating_sub(mana_cost);
                0
            }
        }
    }
}
