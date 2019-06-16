export const abilityModifier = (stat) => {
  if (stat < 3) throw new Error('Ability scores must be at least 3');
  if (stat > 18) throw new Error('Ability scores can be at most 18');

  return Math.floor((stat - 10) / 2);
};

export class Character {
  static rollAbility() {
    return [1, 2, 3, 4]
        .map(() => Math.floor(Math.random() * 6) + 1)
        .sort((a, b) => a - b)
        .splice(0, 3)
        .reduce((sum, num) => sum + num, 0);
  }

  constructor() {
    this.charisma = Character.rollAbility();
    this.constitution = Character.rollAbility();
    this.dexterity = Character.rollAbility();
    this.intelligence = Character.rollAbility();
    this.strength = Character.rollAbility();
    this.wisdom = Character.rollAbility();
    this.hitpoints = 10 + abilityModifier(this.constitution);
  }
}
