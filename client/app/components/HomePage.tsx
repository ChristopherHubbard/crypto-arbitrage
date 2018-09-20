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
import { SvgProperties } from 'csstype';

interface HomeProps
{
    user: User,
    type: string
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
                {
                    date: new Date(2018, 2, 2),
                    open: 5000,
                    high: 5100,
                    low: 4900,
                    close: 4990,
                    volume: 384
                },
                {
                    date: new Date(2018, 2, 3),
                    open: 4990,
                    high: 9000,
                    low: 400,
                    close: 6990,
                    volume: 500
                },
                {
                    date: new Date(2018, 2, 4),
                    open: 500,
                    high: 510,
                    low: 490,
                    close: 499,
                    volume: 10
                },
                {
                    date: new Date(2018, 2, 5),
                    open: 4990,
                    high: 9000,
                    low: 300,
                    close: 6990,
                    volume: 300
                },
                {
                    date: new Date(2018, 2, 6),
                    open: 5990,
                    high: 6400,
                    low: 300,
                    close: 5990,
                    volume: 250
                }
            ]
        }
    }

    public render(): React.ReactNode
    {
        // Extract prop data
        const { type } = this.props;

        // Extract the data from the state
        const { data: initialData } = this.state;

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

        const smaVolume50 = sma()
            .id(3)
            .options({
                windowSize: 50,
                sourcePath: "volume",
            })
            .merge((d: any, c: any) => {d.smaVolume50 = c;})
            .accessor((d: any) => d.smaVolume50);
        
        const macdCalculator = macd()
            .options({
                fast: 12,
                slow: 26,
                signal: 9,
            })
            .merge((d: any, c: any) => {d.macd = c;})
            .accessor((d: any) => d.macd);

        const calculatedData = smaVolume50(macdCalculator(ema12(ema26(initialData))));
        const xScaleProvider = discontinuousTimeScaleProvider.inputDateAccessor((d: any) => d.date);
        const { data, xScale, xAccessor, displayXAccessor } = xScaleProvider(calculatedData);
            
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
                <ChartCanvas height={600}
                    width={1200}
                    ratio={0.1}
                    margin={{ left: 70, right: 70, top: 20, bottom: 30 }}
                    type={type}
                    seriesName="Arbitrage"
                    data={data}
                    xScale={xScale}
                    xAccessor={xAccessor}
                    displayXAccessor={displayXAccessor}
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

                        <CandlestickSeries/>
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
                        yExtents={[(d: any) => d.volume, smaVolume50.accessor()]}
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
                        <AreaSeries yAccessor={smaVolume50.accessor()} stroke={smaVolume50.stroke()} fill={smaVolume50.fill()}/>
                    </Chart>
                    <CrossHairCursor />
                </ChartCanvas>
            </div>
        );
    }
}

function mapStateToProps(state: any): HomeProps
{
    const { user } = state.authentication;
    const type = "svg";
    return {
        user,
        type
    };
}

export default connect<HomeProps>(
    mapStateToProps
)(fitWidth(HomePage));