import { Component, ElementRef, ViewChild} from '@angular/core';

import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-status-map',
    templateUrl: './status-map.component.html',
    styleUrls: ['./status-map.component.css']
})
export class StatusMapComponent {
    @ViewChild('statusMapCanvas') statusMapCanvas: ElementRef;

    // position on canvas that translates to the 0 position in antennaPositions
    mapZeroXCanvasPosition: number = 250;
    mapZeroYCanvasPosition: number = 250;
    // radius of antenna marker
    antennaRadius: number = 8;
    // canvas padding
    canvasPaddingX: number = 12;
    canvasPaddingY: number = 12;

    constructor(private tartService: TartService) { }

    ngAfterViewInit() {
        let antennaPos = this.getAntennaPositions();
        this.drawAntennaPositions(antennaPos);
    }

    drawAntennaPositions(antennaPositions) {
        /**
         * First thing to do is to draw the smallest possible thing at
         * each antenna position
         */
        let drawCanvas = this.statusMapCanvas.nativeElement;
        let ctx = drawCanvas.getContext('2d');
        let xCoordIndex = 0;
        let yCoordIndex = 1;
        let numAntennaPos = antennaPositions.length;
        // calculate maximum usable area in map (should be (size / 2) - some offset  )
        let xBounds = (drawCanvas.width / 2) - this.canvasPaddingX;
        let yBounds = (drawCanvas.height / 2) - this.canvasPaddingY;
        // calculate max and min x and y position
        let minX = antennaPositions[0][xCoordIndex];
        let maxX = antennaPositions[0][xCoordIndex];

        let minY = antennaPositions[0][yCoordIndex];
        let maxY = antennaPositions[0][yCoordIndex];

        for (let i = 1; i < numAntennaPos; i++) {
            if (antennaPositions[i][xCoordIndex] < minX) {
                minX = antennaPositions[i][xCoordIndex];
            } else if (antennaPositions[i][xCoordIndex] > maxX) {
                maxX = antennaPositions[i][xCoordIndex];
            }

            if (antennaPositions[i][yCoordIndex] < minY) {
                minY = antennaPositions[i][yCoordIndex];
            } else if (antennaPositions[i][yCoordIndex] > maxY) {
                maxY = antennaPositions[i][yCoordIndex];
            }
        }
        // calculate max in x and y position
        if (minX < 0) {
            minX *= -1;
            maxX = Math.max(minX, maxX);
        }
        if (minY < 0) {
            minY *= -1;
            maxY = Math.max(minY, maxY);
        }
        // calculate x and y position modifiers (I think I go maxUsableArea / maxX)
        let xMod = xBounds / (maxX);
        let yMod = yBounds / (maxY);

        antennaPositions.forEach(antenna => {
            let xPos = (xMod * antenna[xCoordIndex]) + this.mapZeroXCanvasPosition;
            let yPos = (yMod * antenna[yCoordIndex]) + this.mapZeroYCanvasPosition;

            ctx.beginPath();
            ctx.fillStyle = "rgba(0, 255, 0, 0.5)";
            ctx.ellipse(xPos , yPos, this.antennaRadius, this.antennaRadius,
                Math.PI / 180, 0, 2 * Math.PI);
            ctx.fill();
            ctx.beginPath();
            ctx.ellipse(xPos , yPos, this.antennaRadius, this.antennaRadius,
                Math.PI / 180, 0, 2 * Math.PI);
            ctx.stroke();
        });
    }

    getAntennaPositions() {
        return [
            [-0.29, 0.79, 0.0],
            [0.03, 0.81, 0.0],
            [-0.4, 0.64, 0.0],
            [0.39, 1.31, 0.0],
            [-0.34, 1.37, 0.0],
            [0.12, 1.4, 0.0],
            [1.06, -0.36, 0.0],
            [1.2, -0.07, 0.0],
            [1.13, -0.53, 0.0],
            [0.95, 0.49, 0.0],
            [0.53, -0.11, 0.0],
            [0.73, 0.3, 0.0],
            [-0.36, -1.06, 0.0],
            [-0.07, -1.2, 0.0],
            [-0.53, -1.13, 0.0],
            [0.49, -0.95, 0.0],
            [-0.11, -0.53, 0.0],
            [0.3, -0.73, 0.0],
            [-0.87, 0.34, 0.0],
            [-1.14, 0.16, 0.0],
            [-0.85, 0.52, 0.0],
            [-1.2, -0.45, 0.0],
            [-0.54, -0.14, 0.0],
            [-0.92, -0.4, 0.0]
        ];
    }

    getChannelsStatus() {
        let apiResponse = [
          {
            "id": 0,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.999,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4594,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 1,
            "phase": {
              "N_samples": 150,
              "measured": 5,
              "ok": 1,
              "stability": 0.999,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4995,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 2,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.971,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4943,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 3,
            "phase": {
              "N_samples": 150,
              "measured": 3,
              "ok": 1,
              "stability": 0.976,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4948,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 4,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.982,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5881,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 5,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.984,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5803,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 6,
            "phase": {
              "N_samples": 150,
              "measured": 6,
              "ok": 1,
              "stability": 0.991,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5126,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 7,
            "phase": {
              "N_samples": 150,
              "measured": 5,
              "ok": 1,
              "stability": 0.994,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4926,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 8,
            "phase": {
              "N_samples": 150,
              "measured": 5,
              "ok": 1,
              "stability": 0.999,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5061,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 9,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.969,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.6018,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 10,
            "phase": {
              "N_samples": 150,
              "measured": 5,
              "ok": 1,
              "stability": 0.997,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4873,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 11,
            "phase": {
              "N_samples": 150,
              "measured": 6,
              "ok": 1,
              "stability": 0.977,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.477,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 12,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.998,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4831,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 13,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.993,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5559,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 14,
            "phase": {
              "N_samples": 150,
              "measured": 3,
              "ok": 1,
              "stability": 0.966,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5053,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 15,
            "phase": {
              "N_samples": 150,
              "measured": 3,
              "ok": 1,
              "stability": 0.999,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.5295,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 16,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.993,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4885,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 17,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.966,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.7368,
              "ok": 0,
              "threshold": 0.2
            }
          },
          {
            "id": 18,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.975,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.6191,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 19,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.966,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.729,
              "ok": 0,
              "threshold": 0.2
            }
          },
          {
            "id": 20,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.999,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4963,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 21,
            "phase": {
              "N_samples": 150,
              "measured": 3,
              "ok": 1,
              "stability": 1.0,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.4858,
              "ok": 1,
              "threshold": 0.2
            }
          },
          {
            "id": 22,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.974,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.7407,
              "ok": 0,
              "threshold": 0.2
            }
          },
          {
            "id": 23,
            "phase": {
              "N_samples": 150,
              "measured": 4,
              "ok": 1,
              "stability": 0.971,
              "threshold": 0.95
            },
            "radio_mean": {
              "mean": 0.6325,
              "ok": 1,
              "threshold": 0.2
            }
          }
        ];
        let serviceResponse = [];
        apiResponse.forEach(channel => {
            this.tartService.createChannelStatus(channel);
        });
        return serviceResponse;
    }
}
