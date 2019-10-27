export default class SpaceAge {
  public readonly seconds: number;

  constructor(seconds: Readonly<number>) {
    this.seconds = seconds;
  }

  private relativeDuration(period: Readonly<number>): number {
    return +(this.seconds / EARTH_YEAR_IN_SECONDS / period).toFixed(2);
  }

  onMercury() {
    return this.relativeDuration(0.2408467);
  }
  onVenus() {
    return this.relativeDuration(0.61519726);
  }
  onEarth() {
    return this.relativeDuration(1);
  }
  onMars() {
    return this.relativeDuration(1.8808158);
  }
  onJupiter() {
    return this.relativeDuration(11.862615);
  }
  onSaturn() {
    return this.relativeDuration(29.447498);
  }
  onNeptune() {
    return this.relativeDuration(164.79132);
  }
  onUranus() {
    return this.relativeDuration(84.016846);
  }
}

const EARTH_YEAR_IN_SECONDS = 31557600;
