const MINUTES_IN_HOUR = 60;
const HOURS_IN_DAY = 24;
const MINUTES_IN_DAY = MINUTES_IN_HOUR * HOURS_IN_DAY;
const TIME_FIELD_WIDTH = 2; // 12:23, each field is 2 wide
const TIME_FIELD_PAD = "0"; // 02:00, pad with 0

export default class Clock {
  private minutes = 0;

  constructor(hours: number, minutes = 0) {
    this.setNormalizedMinutes(hours * MINUTES_IN_HOUR + minutes);
  }

  plus(minutes: number): Clock {
    this.setNormalizedMinutes(this.minutes + minutes);
    return this;
  }

  minus(minutes: number): Clock {
    this.setNormalizedMinutes(this.minutes - minutes);
    return this;
  }

  equals(other: Clock): boolean {
    return other.minutes === this.minutes;
  }

  private setNormalizedMinutes(minutes: number): void {
    this.minutes = remainder(minutes, MINUTES_IN_DAY);
  }

  toString(): string {
    const hours = formatNumber(Math.trunc(this.minutes / MINUTES_IN_HOUR));
    const minutes = formatNumber(this.minutes % MINUTES_IN_HOUR);

    return `${hours}:${minutes}`;
  }
}

function remainder(dividend: number, divisor: number): number {
  return ((dividend % divisor) + divisor) % divisor;
}

function formatNumber(num: number): string {
  return String(num).padStart(TIME_FIELD_WIDTH, TIME_FIELD_PAD);
}
