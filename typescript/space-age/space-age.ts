const EARTH_YEAR_IN_SECONDS = 31557600;

export default class SpaceAge {
  public readonly seconds: number;

  constructor(seconds: Readonly<number>) {
    this.seconds = seconds;
  }

  private relativeDuration(period: Readonly<number>): number {
    return +(this.seconds / EARTH_YEAR_IN_SECONDS / period).toFixed(2);
  }

  onMercury(): number {
    return this.relativeDuration(0.2408467);
  }
  onVenus(): number {
    return this.relativeDuration(0.61519726);
  }
  onEarth(): number {
    return this.relativeDuration(1);
  }
  onMars(): number {
    return this.relativeDuration(1.8808158);
  }
  onJupiter(): number {
    return this.relativeDuration(11.862615);
  }
  onSaturn(): number {
    return this.relativeDuration(29.447498);
  }
  onNeptune(): number {
    return this.relativeDuration(164.79132);
  }
  onUranus(): number {
    return this.relativeDuration(84.016846);
  }
}
