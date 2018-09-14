import * as React from 'react';
import { connect, DispatchProp } from 'react-redux';
import { format, scaleTime } from 'd3';

// Can I combine these imports so that they use only 'react-stockcharts' instead of nested folders??
import { ChartCanvas, Chart } from "react-stockcharts";
import {
	BarSeries,
	AreaSeries,
	CandlestickSeries,
	LineSeries,
	MACDSeries,
} from "react-stockcharts/lib/series";
import { XAxis, YAxis } from "react-stockcharts/lib/axes";
import {
	CrossHairCursor,
	EdgeIndicator,
	CurrentCoordinate,
	MouseCoordinateX,
	MouseCoordinateY,
} from "react-stockcharts/lib/coordinates";

import { discontinuousTimeScaleProvider } from "react-stockcharts/lib/scale";
import {
	OHLCTooltip,
	MovingAverageTooltip,
	MACDTooltip,
} from "react-stockcharts/lib/tooltip";
import { ema, macd, sma } from "react-stockcharts/lib/indicator";
import { fitWidth } from "react-stockcharts/lib/helper";
import { User } from '../models';

interface HomeProps
{
    user: User
}

interface HomeState
{
    data: Array<any>
}

class HomePage extends React.Component<HomeProps & DispatchProp<any>, HomeState>
{
    constructor(props: HomeProps & DispatchProp<any>)
    {
        super(props);

        // Set the initial data??
        this.state = {
            data: [
            ]
        }
    }

    public render(): React.ReactNode
    {
        // Extract prop data
        //const { width, ratio } = this.props;
        // Extract the data from the state
        const { data } = this.state;

        // Set up the EMA12 and EMA26 lines
        const ema26 = ema()
			.id(0)
			.options({ windowSize: 26 })
			.merge((d: any, c: any) => { d.ema26 = c; })
			.accessor((d: any) => d.ema26);

		const ema12 = ema()
			.id(1)
			.options({ windowSize: 12 })
			.merge((d: any, c: any) => {d.ema12 = c;})
            .accessor((d: any) => d.ema12);
            
        // Create the options for the candlechart -- make it waterfall
        const mouseEdgeAppearance = {
            textFill: "#542605",
            stroke: "#05233B",
            strokeOpacity: 1,
            strokeWidth: 3,
            arrowWidth: 5,
            fill: "#BCDEFA",
        };

        return (
            <div>
                <h1> Home Page </h1>
                <ChartCanvas height={600} width={1200} ratio={2} xScale={scaleTime()}
				    margin={{ left: 70, right: 70, top: 20, bottom: 30 }}
				    seriesName="MSFT"
				    data={data}
                >
                    <Chart id={1} height={400}
                        yExtents={[(d: any) => [d.high, d.low], ema26.accessor(), ema12.accessor()]}
                        padding={{ top: 10, bottom: 20 }}
                    >
                        <XAxis axisAt="bottom" orient="bottom" showTicks={false} outerTickSize={0} />
                        <YAxis axisAt="right" orient="right" ticks={5} />

                        <MouseCoordinateY
                            at="right"
                            orient="right"
                            displayFormat={format(".2f")}
                            {...mouseEdgeAppearance}
                        />

                        <CandlestickSeries />
                        <LineSeries yAccessor={ema26.accessor()} stroke={ema26.stroke()}/>
                        <LineSeries yAccessor={ema12.accessor()} stroke={ema12.stroke()}/>

                        <CurrentCoordinate yAccessor={ema26.accessor()} fill={ema26.stroke()} />
                        <CurrentCoordinate yAccessor={ema12.accessor()} fill={ema12.stroke()} />

                        <EdgeIndicator itemType="last" orient="right" edgeAt="right"
                            yAccessor={(d: any) => d.close}
                            fill={(d: any) => d.close > d.open ? "#A2F5BF" : "#F9ACAA"}
                            stroke={(d: any) => d.close > d.open ? "#0B4228" : "#6A1B19"}
                            textFill={(d: any) => d.close > d.open ? "#0B4228" : "#420806"}
                            strokeOpacity={1}
                            strokeWidth={3}
                            arrowWidth={2}
                        />

                        <OHLCTooltip origin={[-40, 0]}/>
                        <MovingAverageTooltip
                            onClick={(e: any) => console.log(e)}
                            origin={[-38, 15]}
                            options={[
                                {
                                    yAccessor: ema26.accessor(),
                                    type: "EMA",
                                    stroke: ema26.stroke(),
                                    windowSize: ema26.options().windowSize,
                                },
                                {
                                    yAccessor: ema12.accessor(),
                                    type: "EMA",
                                    stroke: ema12.stroke(),
                                    windowSize: ema12.options().windowSize,
                                },
                            ]}
                        />
                    </Chart>
                    <Chart id={2} height={150}
                        yExtents={[(d: any) => d.volume]}
                        origin={(w: any, h: any) => [0, h - 300]}
                    >
                        <YAxis axisAt="left" orient="left" ticks={5} tickFormat={format(".2s")}/>

                        <MouseCoordinateY
                            at="left"
                            orient="left"
                            displayFormat={format(".4s")}
                            {...mouseEdgeAppearance}
                        />

                        <BarSeries yAccessor={(d: any) => d.volume} fill={(d: any) => d.close > d.open ? "#6BA583" : "#FF0000"} />
                    </Chart>
                    <CrossHairCursor/>
                </ChartCanvas>
            </div>
        );
    }
}

function mapStateToProps(state: any): HomeProps
{
    const { user } = state.authentication;
    return {
        user
    };
}

export default connect<HomeProps>(
    mapStateToProps
)(HomePage);