import { TartAppPage } from './app.po';

describe('tart-app App', function() {
  let page: TartAppPage;

  beforeEach(() => {
    page = new TartAppPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
